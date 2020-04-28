#include <algorithm>
#include <chrono>
#include <string>
#include <vector>

// util
#include "util/dice_roll.hpp"
#include "util/pseudo_random.hpp"
#include "util/stats.hpp"

// caf
#include "caf/all.hpp"
#include "caf/io/middleman.hpp"

/// clients actions
enum class action : uint8_t { post, leave, invite, compute, none };

namespace std {

template <>
struct hash<action> {
  size_t operator()(const action& x) const {
    std::hash<uint8_t> h;
    return h(static_cast<uint8_t>(x));
  }
};

} // namespace std

// types
using client_seq = std::vector<caf::actor>;
using chat_seq = std::vector<caf::actor>;
using action_map = std::unordered_map<action, uint64_t>;

using payload = std::vector<uint8_t>;

// messages
using post_atom = caf::atom_constant<caf::atom("post")>;
using forward_atom = caf::atom_constant<caf::atom("forward")>;
using bump_atom = caf::atom_constant<caf::atom("bump")>;
using stop_atom = caf::atom_constant<caf::atom("stop")>;
using join_atom = caf::atom_constant<caf::atom("join")>;
using leave_atom = caf::atom_constant<caf::atom("leave")>;
using left_atom = caf::atom_constant<caf::atom("left")>;
using befriend_atom = caf::atom_constant<caf::atom("befriend")>;
using logout_atom = caf::atom_constant<caf::atom("logout")>;
using invite_atom = caf::atom_constant<caf::atom("invite")>;
using act_atom = caf::atom_constant<caf::atom("act")>;
using login_atom = caf::atom_constant<caf::atom("login")>;
using finished_atom = caf::atom_constant<caf::atom("finished")>;
using poke_atom = caf::atom_constant<caf::atom("poke")>;
using disconnect_atom = caf::atom_constant<caf::atom("disconnect")>;
using confirm_atom = caf::atom_constant<caf::atom("confirm")>;
using print_atom = caf::atom_constant<caf::atom("print")>;
using collect_atom = caf::atom_constant<caf::atom("collect")>;
using apply_atom = caf::atom_constant<caf::atom("apply")>;
using complete_atom = caf::atom_constant<caf::atom("complete")>;
using append_atom = caf::atom_constant<caf::atom("append")>;
using quit_atom = caf::atom_constant<caf::atom("quit")>;

/// simulates extern client events for each turn
struct behavior_factory {
  behavior_factory() = default;

  behavior_factory(uint32_t compute, uint32_t post, uint32_t leave,
                   uint32_t invite)
    : _compute(compute),
      _post(_compute + post),
      _leave(_post + leave),
      _invite(_leave + invite) {
    // nop
  }

  behavior_factory(const behavior_factory& f) = default;

  action apply(dice_roll dice) const {
    auto pick = dice.apply();
    auto next_action = action::none;
    if (pick < _compute)
      next_action = action::compute;
    else if (pick < _post)
      next_action = action::post;
    else if (pick < _leave)
      next_action = action::leave;
    else if (pick < _invite)
      next_action = action::invite;
    return next_action;
  }

  uint32_t _compute;
  uint32_t _post;
  uint32_t _leave;
  uint32_t _invite;
};

template <class Inspector>
typename Inspector::result_type inspect(Inspector& f, behavior_factory& x) {
  return f(caf::meta::type_name("behavior_factory"), x._compute, x._post,
           x._leave, x._invite);
}

/// for clients compute turn
uint64_t fibonacci(uint8_t x) {
  if (x == 0 || x == 1) {
    return x;
  } else {
    auto j = x / 2;
    auto fib_j = fibonacci(j);
    auto fib_i = fibonacci(j - 1);
    if (x % 2 == 0) {
      return fib_j * (fib_j + (fib_i * 2));
    } else if (x % 4 == 1) {
      return (((fib_j * 2) + fib_i) * ((fib_j * 2) - fib_i)) + 2;
    } else {
      return (((fib_j * 2) + fib_i) * ((fib_j * 2) - fib_i)) - 2;
    }
  }
}

struct chat_state {
  client_seq members;
  std::vector<payload> buffer;
  const char* name = "chat";
};

caf::behavior
chat(caf::stateful_actor<chat_state>* self, const caf::actor initiator) {
  self->state.members.emplace_back(initiator);
  // TODO: Check if all messages are handled.
  self->set_default_handler(caf::print_and_drop);
  return {
    [=](post_atom, payload& pl, const caf::actor& accumulator) {
      auto& s = self->state;
#ifndef BENCH_NO_BUFFERED_CHATS
      s.buffer.push_back(pl);
#endif
      if (s.members.empty()) {
        self->send(accumulator, stop_atom::value, action::post);
      } else {
        self->send(accumulator, bump_atom::value, s.members.size());
        auto msg = caf::make_message(forward_atom::value, self, std::move(pl),
                                     accumulator);
        for (auto& member : s.members)
          self->send(member, msg);
      }
    },
    [=](join_atom, const caf::actor& client, const caf::actor& accumulator) {
      auto& s = self->state;
      s.members.emplace_back(client);
#ifdef BENCH_NO_BUFFERED_CHATS
      self->send(accumulator, stop_atom::value, action::invite);
#else
      if (s.buffer.empty()) {
        self->send(accumulator, stop_atom::value, action::invite);
      } else {
        self->send(accumulator, bump_atom::value, s.buffer.size());
        for (auto& message : s.buffer)
          self->send(client, forward_atom::value, self, message, accumulator);
      }
#endif
    },
    [=](leave_atom, const caf::actor& client, const bool did_logout,
        const caf::actor& accumulator) {
      auto& s = self->state;
      auto itr = std::find(s.members.begin(), s.members.end(), client);
      if (itr != s.members.end())
        s.members.erase(itr);
      self->send(client, left_atom::value, self, did_logout, accumulator);
      // TODO: Is this correct?
      if (s.members.empty())
        self->quit();
    },
  };
}

struct client_state {
  uint64_t id;
  client_seq friends;
  chat_seq chats;
  caf::actor directory;
  dice_roll dice;
  pseudo_random rand;
  const char* name = "client";
};

caf::behavior client(caf::stateful_actor<client_state>* self, const uint64_t id,
                     const caf::actor directory, uint64_t seed) {
  auto& s = self->state;
  s.id = id;
  s.directory = directory;
  s.rand = pseudo_random(seed);
  s.dice = dice_roll(s.rand);
  self->set_default_handler(caf::print_and_drop);
  return {
    [=](befriend_atom, const caf::actor& client) {
      self->state.friends.emplace_back(client);
    },
    [=](logout_atom) {
      auto& s = self->state;
      if (s.chats.empty()) {
        self->send(s.directory, left_atom::value, self);
        self->quit();
      } else
        for (auto& chat : s.chats)
          self->send(chat, leave_atom::value, self, true, caf::actor{});
    },
    [=](left_atom, const caf::actor& chat, const bool did_logout,
        const caf::actor& accumulator) {
      auto& s = self->state;
      auto itr = std::find(s.chats.begin(), s.chats.end(), chat);
      if (itr != s.chats.end())
        s.chats.erase(itr);
      if (did_logout && s.chats.empty()) {
        self->send(s.directory, left_atom::value, self);
        self->quit(); // TODO ask pony about this
      } else if (accumulator) {
        self->send(accumulator, stop_atom::value, action::leave);
      }
    },
    [=](invite_atom, const caf::actor& chat, const caf::actor& accumulator) {
      self->state.chats.emplace_back(chat);
      self->send(chat, join_atom::value, self, accumulator);
    },
    [=](forward_atom, const caf::actor&, const payload&,
        const caf::actor& accumulator) {
      self->send(accumulator, stop_atom::value, action::post);
    },
    [=](act_atom, behavior_factory& behavior, const caf::actor& accumulator) {
      auto& s = self->state;
      size_t index = s.rand.next_int(s.chats.size());
      switch (behavior.apply(s.dice)) {
        case action::post:
          if (!s.chats.empty())
            self->send(s.chats[index], post_atom::value, payload{},
                       accumulator);
          else
            self->send(accumulator, stop_atom::value, action::none);
          break;
        case action::leave:
          if (!s.chats.empty())
            self->send(s.chats[index], leave_atom::value, self, false,
                       accumulator);
          else
            self->send(accumulator, stop_atom::value, action::none);
          break;
        case action::compute:
          fibonacci(35);
          self->send(accumulator, stop_atom::value, action::compute);
          break;
        case action::invite: {
          auto created = self->spawn(chat, self);
          s.chats.emplace_back(created);
          std::vector<caf::actor> f(s.friends.size());
          std::copy(s.friends.begin(), s.friends.end(), f.begin());
          s.rand.shuffle(f);
          auto invitations
            = s.friends.empty()
                ? 0
                : static_cast<size_t>(s.rand.next_long() % s.friends.size());
          if (invitations == 0) {
            self->send(accumulator, stop_atom::value, action::invite);
          } else {
            self->send(accumulator, bump_atom::value, invitations);
            for (size_t i = 0; i < invitations; ++i)
              self->send(f[i], invite_atom::value, created, accumulator);
          }
          break;
        }
        default: // case action::none:
          self->send(accumulator, stop_atom::value, action::none);
          break;
      }
    },
  };
}

struct directory_state {
  client_seq clients;
  pseudo_random random;
  uint32_t befriend;
  caf::actor poker;
  const char* name = "directory";
};

caf::behavior directory(caf::stateful_actor<directory_state>* self,
                        uint64_t seed, uint32_t befriend) {
  auto& s = self->state;
  s.random = pseudo_random(seed);
  s.befriend = befriend;
  self->set_default_handler(caf::print_and_drop);
  return {
    [=](login_atom, uint64_t id) {
      auto& s = self->state;
      auto new_client = self->spawn(client, id, self, s.random.next_long());
      s.clients.emplace_back(new_client);
      for (auto& client : s.clients) {
        if (s.random.next_int(100) < s.befriend) {
          self->send(client, befriend_atom::value, new_client);
          self->send(new_client, befriend_atom::value, client);
        }
      }
    },
    [=](left_atom, caf::actor& client) {
      auto& s = self->state;
      auto itr = std::find(s.clients.begin(), s.clients.end(), client);
      if (itr != s.clients.end())
        s.clients.erase(itr);
      if (s.clients.empty())
        self->send(s.poker, finished_atom::value);
    },
    [=](poke_atom, behavior_factory& behavior, const caf::actor& accumulator) {
      for (auto& client : self->state.clients)
        self->send(client, act_atom::value, behavior, accumulator);
    },
    [=](disconnect_atom, caf::actor& poker) {
      auto& s = self->state;
      s.poker = poker;
      for (auto& client : s.clients)
        self->send(client, logout_atom::value);
    },
    [=](quit_atom) { self->quit(); },
  };
}

using time_point = std::chrono::time_point<std::chrono::high_resolution_clock>;
struct accumulator_state {
  caf::actor poker;
  action_map actions;
  time_point start;
  time_point end;
  /// time in milliseconds
  double duration;
  size_t expected;
  bool did_stop;
  const char* name = "accumulator";
};

caf::behavior accumulator(caf::stateful_actor<accumulator_state>* self,
                          const caf::actor poker, size_t expected) {
  auto& s = self->state;
  s.poker = poker;
  s.start = std::chrono::high_resolution_clock::now();
  s.expected = expected;
  s.did_stop = false;
  self->set_default_handler(caf::print_and_drop);
  return {
    [=](bump_atom, const size_t expected) {
      auto& s = self->state;
      s.expected = (s.expected + expected) - 1;
    },
    [=](stop_atom, const action act) {
      auto& s = self->state;
      ++s.actions[act];
      --s.expected;
      if (s.expected == 0) {
        s.end = std::chrono::high_resolution_clock::now();
        s.duration = std::chrono::duration_cast<std::chrono::milliseconds>(
                       s.end - s.start)
                       .count();
        s.did_stop = true;
        self->send(s.poker, confirm_atom::value);
      }
    },
    [=](print_atom, const caf::actor& poker, size_t i, size_t j) {
      self->send(poker, collect_atom::value, i, j, self->state.duration,
                 self->state.actions);
      self->quit();
    },
  };
}

struct poker_state {
  action_map actions;
  uint64_t clients;
  size_t logouts;
  size_t confirmations;
  uint64_t turns;
  size_t iteration;
  std::vector<caf::actor> directories;
  std::vector<caf::actor> runtimes;
  size_t accumulations;
  std::vector<std::vector<double>> finals;
  behavior_factory factory;
  caf::actor bench;
  bool last;
  std::vector<double> turn_series;
  const char* name = "poker";
};

caf::behavior
poker(caf::stateful_actor<poker_state>* self, uint64_t clients, uint64_t turns,
      size_t directories, uint64_t befriend, behavior_factory factory) {
  auto& s = self->state;
  s.clients = clients;
  s.logouts = 0;
  s.confirmations = 0;
  s.turns = turns;
  s.iteration = 0;
  s.factory = factory;

  auto rand = pseudo_random(42);
  for (size_t i = 0; i < directories; ++i)
    s.directories.emplace_back(
      self->spawn(directory, rand.next_int(), befriend));
  self->set_default_handler(caf::print_and_drop);
  return {
    [=](apply_atom, caf::actor& bench, bool last) {
      auto& s = self->state;
      s.confirmations = static_cast<size_t>(s.turns);
      s.logouts = s.directories.size();
      s.bench = bench;
      s.last = last;
      s.accumulations = 0;

      size_t index = 0;
      std::vector<double> values(s.turns, 0);

      s.finals.emplace_back(std::move(values));

      for (size_t client = 0; client < s.clients; ++client) {
        index = client % s.directories.size();
        self->send(s.directories[index], login_atom::value, client);
      }
      // feedback loop?
      for (uint64_t i = 0; i < s.turns; ++i) {
        auto accu
          = self->spawn(accumulator, self, static_cast<size_t>(s.clients));
        for (auto& directory : s.directories)
          self->send(directory, poke_atom::value, s.factory, accu);
        s.runtimes.push_back(accu);
      }
    },
    [=](confirm_atom) {
      auto& s = self->state;
      --s.confirmations;
      if (s.confirmations == 0)
        for (auto& d : s.directories)
          self->send(d, disconnect_atom::value, self);
    },
    [=](finished_atom) {
      auto& s = self->state;
      --s.logouts;
      if (s.logouts == 0) {
        size_t turn = 0;
        for (auto& accumulator : s.runtimes) {
          ++s.accumulations;
          self->send(accumulator, print_atom::value, self, s.iteration, turn);
          ++turn;
        }
        s.runtimes.clear();
      }
    },
    [=](collect_atom, size_t i, size_t j, double duration,
        const action_map& actions) {
      auto& s = self->state;
      for (auto& act : actions)
        s.actions[act.first] += act.second; // act.first += act.second;
      try {
        s.finals.at(i).at(j) = duration;
        s.turn_series.push_back(duration);
      } catch (std::exception& e) {
        aout(self) << "Data collection failed to access element at i=" << i
                   << ", j=" << j << std::endl;
      }

      --s.accumulations;
      if (s.accumulations == 0) {
        ++s.iteration;
        if (s.bench) {
          self->send(s.bench, complete_atom::value);

          if (s.last) {
            sample_stats stats(s.turn_series);
            std::vector<std::vector<double>> turns;
            std::vector<double> qos;

            // TODO Ask pony about line 381 to 391.

            for (size_t l = 0; l < s.finals.size(); ++l) {
              qos.push_back(sample_stats(s.finals.back()).stddev());
              s.finals.pop_back();
            }

            std::stringstream title_text;
            title_text << std::string(31, ' ') << std::setw(18) << "j-mean"
                       << std::setw(18) << "j-median" << std::setw(18)
                       << "j-error" << std::setw(18) << "j-stddev"
                       << std::setw(32) << "quality of service" << std::endl;

            std::stringstream result_text;
            result_text << "Turns" << std::string(27, ' ') << std::setw(17)
                        << stats.mean() << " " << std::setw(17)
                        << stats.median() << " " << std::setw(17) << stats.err()
                        << " " << std::setw(17) << stats.stddev() << " "
                        << std::setw(31) << sample_stats(qos).median()
                        << std::endl;

            std::stringstream act_text;
            act_text << std::endl
                     << "Acts:" << std::endl
                     << "Post: " << s.actions[action::post] << std::endl
                     << "Leave: " << s.actions[action::leave] << std::endl
                     << "Invite: " << s.actions[action::invite] << std::endl
                     << "Compute: " << s.actions[action::compute] << std::endl
                     << "None: " << s.actions[action::none] << std::endl;

            self->send(s.bench, append_atom::value, title_text.str(),
                       result_text.str(), act_text.str());
            for (auto& d : s.directories)
              self->send(d, quit_atom::value);
            self->quit();
          }
        }
      }
    },
    [=](quit_atom) { self->quit(); },
  };
}

struct config : caf::actor_system_config {
  uint64_t run = 32;
  size_t directories = 8;
  uint64_t clients = 1024;
  uint64_t turns = 32;
  uint64_t compute = 55;
  uint64_t post = 25;
  uint64_t leave = 10;
  uint64_t invite = 10;
  uint64_t befriend = 10;
  config() {
    add_message_type<std::vector<uint8_t>>("std::vector<uint8_t>");
    add_message_type<std::vector<double>>("std::vector<double>");
    add_message_type<size_t>("size_t");
    add_message_type<uint64_t>("uint64_t");
    add_message_type<behavior_factory>("behavior_factory");
    opt_group{custom_options_, "global"}
      .add(run, "run,r", "The number of iterations. Defaults to 32")
      .add(clients, "clients,c", "The number of clients. Defaults to 1024.")
      .add(directories, "directories,d",
           "The number of directories. Defaults to 8.")
      .add(turns, "turns,t", "The number of turns. Defaults to 32.")
      .add(compute, "compute,m",
           "The compute behavior probability. Defaults to 55.")
      .add(post, "post,p", "The post behavior probability. Defaults to 25.")
      .add(leave, "leave,l", "The leave behavior probability. Defaults to 10.")
      .add(invite, "invite,d",
           "The invite behavior probability. Defaults to 10.")
      .add(befriend, "befriend,b", "The befriend probability. Defaults to 10.");
    // TODO: What about the parseable opt?
  }
};

struct chatapp_state {
  const char* name = "chatapp";
};

caf::behavior
chatapp(caf::stateful_actor<chatapp_state>* self, const uint64_t clients,
        const uint64_t turns, const size_t directories, const uint64_t compute,
        const uint64_t post, const uint64_t leave, const uint64_t invite,
        const uint64_t befriend) {
  auto factory = behavior_factory(compute, post, leave, invite);
  auto poke_actor
    = self->spawn(poker, clients, turns, directories, befriend, factory);
  return {
    [=](apply_atom, caf::actor& async_benchmark_completion, bool last) {
      self->send(poke_actor, apply_atom::value, async_benchmark_completion,
                 last);
    },
    [=](quit_atom) {
      self->send(poke_actor, quit_atom::value);
      self->quit();
    },
  };
}

void caf_main(caf::actor_system& system, const config& cfg) {
  if ((cfg.compute + cfg.post + cfg.leave + cfg.invite) != 100) {
    std::cerr << "Invalid arguments! Sum of probabilities != 100." << std::endl;
    return;
  } else {
    auto chat = system.spawn(chatapp, cfg.clients, cfg.turns, cfg.directories,
                             cfg.compute, cfg.post, cfg.leave, cfg.invite,
                             cfg.befriend);
    caf::scoped_actor self{system};
    std::vector<double> durations;
    std::stringstream title_text;
    title_text << std::string(31, ' ') << std::setw(18) << "i-mean"
               << std::setw(18) << "i-median" << std::setw(18) << "i-error"
               << std::setw(18) << "i-stddev" << std::endl;
    std::cout << title_text.str();
    auto perform_run = [&](bool collect_data) {
      auto start = std::chrono::high_resolution_clock::now();
      self->send(chat, apply_atom::value, self, collect_data);
      self->receive([start, &durations](complete_atom) {
        auto end = std::chrono::high_resolution_clock::now();
        durations.emplace_back(
          std::chrono::duration_cast<std::chrono::milliseconds>(end - start)
            .count());
        sample_stats stats(durations);
      });
    };
    for (uint64_t i = 1; i < cfg.run; ++i)
      perform_run(false);
    perform_run(true);
    self->receive([&](append_atom, std::string& title, std::string& result,
                      std::string& act) {
      sample_stats stats(durations);
      std::stringstream result_text;
      result_text << "ChatApp" << std::string(25, ' ') << std::setw(17)
                  << stats.mean() << " " << std::setw(17) << stats.median()
                  << " " << std::setw(17) << stats.err() << " " << std::setw(17)
                  << stats.stddev() << " " << std::setw(17) << std::endl;
      std::cout << result_text.str();
      std::cout << title << result << act;
    });
    self->send(chat, quit_atom::value);
  }
}

CAF_MAIN(caf::io::middleman)
