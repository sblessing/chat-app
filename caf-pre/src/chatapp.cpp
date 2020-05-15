#include <algorithm>
#include <cassert>
#include <chrono>
#include <iomanip>
#include <string>
#include <vector>

// util
#include "util/pseudo_random.hpp"
#include "util/stats.hpp"

// caf
#include "caf/all.hpp"

/// clients actions
enum class action : uint8_t {
  post,
  post_delivery,
  leave,
  invite,
  compute,
  ignore,
  error,
  none
};

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
using time_point = std::chrono::time_point<std::chrono::high_resolution_clock>;

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

  action apply(uint32_t pick) const {
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

CAF_BEGIN_TYPE_ID_BLOCK(chatapp, caf::first_custom_type_id)

  CAF_ADD_TYPE_ID(chatapp, (behavior_factory))
  CAF_ADD_TYPE_ID(chatapp, (action_map))
  CAF_ADD_TYPE_ID(chatapp, (payload))

  CAF_ADD_TYPE_ID(chatapp, (action))

  CAF_ADD_ATOM(chatapp, accepted_atom)
  CAF_ADD_ATOM(chatapp, act_atom)
  CAF_ADD_ATOM(chatapp, append_atom)
  CAF_ADD_ATOM(chatapp, apply_atom)
  CAF_ADD_ATOM(chatapp, befriend_atom)
  CAF_ADD_ATOM(chatapp, bump_atom)
  CAF_ADD_ATOM(chatapp, collect_atom)
  CAF_ADD_ATOM(chatapp, complete_atom)
  CAF_ADD_ATOM(chatapp, confirm_atom)
  CAF_ADD_ATOM(chatapp, disconnect_atom)
  CAF_ADD_ATOM(chatapp, finished_atom)
  CAF_ADD_ATOM(chatapp, left_atom)
  CAF_ADD_ATOM(chatapp, login_atom)
  CAF_ADD_ATOM(chatapp, logout_atom)
  CAF_ADD_ATOM(chatapp, poke_atom)
  CAF_ADD_ATOM(chatapp, post_atom)
  CAF_ADD_ATOM(chatapp, print_atom)
  CAF_ADD_ATOM(chatapp, quit_atom)
  CAF_ADD_ATOM(chatapp, stop_atom)

CAF_END_TYPE_ID_BLOCK(chatapp)

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
  self->set_default_handler(caf::print_and_drop);
  return {
    [=](post_atom, payload& pl, const caf::actor& accumulator) {
      auto& s = self->state;
#ifndef BENCH_NO_BUFFERED_CHATS
      s.buffer.push_back(pl);
#endif
      if (s.members.empty()) {
        self->send(accumulator, stop_atom_v, action::post);
      } else {
        self->send(accumulator, bump_atom_v, action::post,
                   s.members.size());
        auto msg = caf::make_message(caf::forward_atom_v, self, std::move(pl),
                                     accumulator);
        for (auto& member : s.members)
          self->send(member, msg);
      }
    },
    [=](caf::join_atom, const caf::actor& client, const caf::actor& accumulator) {
      auto& s = self->state;
      s.members.emplace_back(client);
#ifndef BENCH_NO_BUFFERED_CHATS
      if (!s.buffer.empty()) {
        self->send(accumulator, bump_atom_v, action::ignore,
                   s.buffer.size());
        for (auto& message : s.buffer)
          self->send(client, caf::forward_atom_v, self, message, accumulator);
      }
#endif
      self->send(client, accepted_atom_v, self, accumulator);
    },
    [=](caf::leave_atom, const caf::actor& client, const bool did_logout,
        const caf::actor& accumulator) {
      auto& s = self->state;
      auto itr = std::find(s.members.begin(), s.members.end(), client);
      if (itr != s.members.end())
        s.members.erase(itr);
      self->send(client, left_atom_v, self, did_logout, accumulator);
    },
  };
}

struct client_state {
  client_seq friends;
  chat_seq chats;
  pseudo_random rand;
  const char* name = "client";
};

caf::behavior
client(caf::stateful_actor<client_state>* self, const uint64_t /*id*/,
       const caf::actor directory, uint64_t seed) {
  auto& s = self->state;
  s.rand = pseudo_random(seed);
  self->set_default_handler(caf::print_and_drop);
  return {
    [=](befriend_atom, const caf::actor& client) {
      self->state.friends.emplace_back(client);
    },
    [=](logout_atom) {
      auto& s = self->state;
      if (s.chats.empty()) {
        self->send(directory, left_atom_v, self);
        self->quit();
      } else
        for (auto& chat : s.chats)
          self->send(chat, caf::leave_atom_v, self, true, caf::actor{});
    },
    [=](left_atom, const caf::actor& chat, const bool did_logout,
        const caf::actor& accumulator) {
      auto& s = self->state;
      auto itr = std::find(s.chats.begin(), s.chats.end(), chat);
      if (itr != s.chats.end())
        s.chats.erase(itr);
      if (did_logout && s.chats.empty()) {
        self->send(directory, left_atom_v, self);
        self->quit();
      } else if (accumulator) {
        self->send(accumulator, stop_atom_v, action::leave);
      }
    },
    [=](accepted_atom, const caf::actor& chat, const caf::actor& accumulator) {
      self->state.chats.emplace_back(chat);
      self->send(accumulator, stop_atom_v, action::ignore);
    },
    [=](caf::forward_atom, const caf::actor&, const payload&,
        const caf::actor& accumulator) {
      self->send(accumulator, stop_atom_v, action::post_delivery);
    },
    [=](act_atom, behavior_factory& factory, const caf::actor& accumulator) {
      auto& s = self->state;
      uint8_t fib_index = 35;
      auto index = static_cast<size_t>(
        s.rand.next_int(static_cast<uint32_t>(s.chats.size())));
      switch (factory.apply(s.rand.next_int(100))) {
        case action::post:
          if (!s.chats.empty())
            self->send(s.chats[index], post_atom_v, payload{},
                       accumulator);
          else
            self->send(accumulator, stop_atom_v, action::none);
          break;
        case action::leave:
          if (!s.chats.empty())
            self->send(s.chats[index], caf::leave_atom_v, self, false,
                       accumulator);
          else
            self->send(accumulator, stop_atom_v, action::none);
          break;
        case action::compute:
	  for(size_t i = 0; i < 10000; ++i) {
            if (fibonacci(fib_index) != 9227465) {
              self->send(accumulator, stop_atom_v, action::error);
	      fib_index = fib_index + 1;
	    }
	  }

	  self->send(accumulator, stop_atom_v, action::compute);
          break;
        case action::invite: {
          assert(s.friends.size() != 0);
          auto created = self->spawn(chat, self);
          s.chats.emplace_back(created);
          std::vector<caf::actor> f(s.friends.size());
          std::copy(s.friends.begin(), s.friends.end(), f.begin());
          s.rand.shuffle(f);
          auto invitations = static_cast<size_t>(
            s.rand.next_int(static_cast<uint32_t>(s.friends.size())));
          if (invitations == 0)
            invitations = 1;
          self->send(accumulator, bump_atom_v, action::invite,
                     invitations);
          for (size_t i = 0; i < invitations; ++i)
            self->send(created, caf::join_atom_v, f[i], accumulator);
          break;
        }
        default: // case action::none:
          assert(s.chats.size() == 0 && s.friends.size() > 0);
          self->send(accumulator, stop_atom_v, action::none);
          break;
      }
    },
  };
}

struct directory_state {
  client_seq clients;
  pseudo_random random;
  caf::actor poker;
  const char* name = "directory";
};

caf::behavior directory(caf::stateful_actor<directory_state>* self,
                        uint64_t seed, uint32_t befriend) {
  self->state.random = pseudo_random(seed);
  self->set_default_handler(caf::print_and_drop);
  return {
    [=](login_atom, uint64_t id) {
      auto& s = self->state;
      s.clients.emplace_back(self->spawn(client, id, self, s.random.next()));
    },
    [=](befriend_atom) {
      auto& s = self->state;
      for (const auto& fclient : s.clients) {
        for (auto found_friend = false; !found_friend;) {
          for (const auto& client : s.clients) {
            if ((s.random.next_int(100) < befriend) and fclient != client) {
              self->send(client, befriend_atom_v, fclient);
              self->send(fclient, befriend_atom_v, client);
              found_friend = true;
            }
          }
        }
      }
    },
    [=](left_atom, caf::actor& client) {
      auto& s = self->state;
      auto itr = std::find(s.clients.begin(), s.clients.end(), client);
      if (itr != s.clients.end())
        s.clients.erase(itr);
      if (s.clients.empty()) {
        self->send(s.poker, finished_atom_v);
        self->quit();
      }
    },
    [=](poke_atom, const behavior_factory& factory,
        const caf::actor& accumulator) {
      for (auto& client : self->state.clients)
        self->send(client, act_atom_v, factory, accumulator);
    },
    [=](disconnect_atom, caf::actor& poker) {
      auto& s = self->state;
      s.poker = poker;
      for (auto& client : s.clients)
        self->send(client, logout_atom_v);
    },
  };
}

struct accumulator_state {
  action_map actions;
  time_point start;
  time_point end;
  // time in milliseconds
  double duration;
  size_t expected;
  bool did_stop;
  const char* name = "accumulator";

  void count(const action act) {
    ++actions[act];
  }
};

caf::behavior accumulator(caf::stateful_actor<accumulator_state>* self,
                          const caf::actor poker, size_t expected) {
  auto& s = self->state;
  s.start = std::chrono::high_resolution_clock::now();
  s.expected = expected;
  s.did_stop = false;
  self->set_default_handler(caf::print_and_drop);
  return {
    [=](bump_atom, const action act, const size_t increase) {
      auto& s = self->state;
      s.count(act);
      s.expected = (s.expected + increase) - 1;
    },
    [=](stop_atom, const action act) {
      auto& s = self->state;
      s.count(act);
      --s.expected;
      if (s.expected == 0) {
        s.end = std::chrono::high_resolution_clock::now();
        s.duration = std::chrono::duration_cast<std::chrono::milliseconds>(
                       s.end - s.start)
                       .count();
        s.did_stop = true;
        self->send(poker, confirm_atom_v);
      }
    },
    [=](print_atom, const caf::actor& collector, size_t i, size_t j) {
      self->send(collector, collect_atom_v, i, j, self->state.duration,
                 self->state.actions);
      self->quit();
    },
  };
}

struct poker_state {
  action_map actions;
  size_t logouts;
  size_t confirmations;
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

caf::behavior poker(caf::stateful_actor<poker_state>* self, uint64_t clients,
                    uint64_t turns, size_t num_directories, uint64_t befriend,
                    behavior_factory factory, bool parseable) {
  auto& s = self->state;
  s.logouts = 0;
  s.confirmations = 0;
  s.iteration = 0;

  self->set_default_handler(caf::print_and_drop);
  return {
    [=](apply_atom, caf::actor& bench, bool last) {
      auto& s = self->state;
      auto rand = pseudo_random(42);
      s.directories.clear();
      s.directories.reserve(num_directories);
      for (size_t i = 0; i < num_directories; ++i)
        s.directories.emplace_back(self->spawn(directory, rand.next(), befriend));
      s.confirmations = static_cast<size_t>(turns);
      s.logouts = s.directories.size();
      s.bench = bench;
      s.last = last;
      s.accumulations = 0;

      size_t index = 0;
      std::vector<double> values(turns, 0);

      s.finals.emplace_back(std::move(values));

      for (uint64_t client = 0; client < clients; ++client) {
        index = client % s.directories.size();
        self->send(s.directories[index], login_atom_v, client);
      }
      // To make sure that nobody's friendset is empty
      if (befriend > 0)
        for (const auto& dir : s.directories)
          self->send(dir, befriend_atom_v);
      for (uint64_t i = 0; i < turns; ++i) {
        auto accu
          = self->spawn(accumulator, self, static_cast<size_t>(clients));
        for (const auto& directory : s.directories)
          self->send(directory, poke_atom_v, factory, accu);
        s.runtimes.push_back(accu);
      }
    },
    [=](confirm_atom) {
      auto& s = self->state;
      --s.confirmations;
      if (s.confirmations == 0)
        for (auto& d : s.directories)
          self->send(d, disconnect_atom_v, self);
    },
    [=](finished_atom) {
      auto& s = self->state;
      --s.logouts;
      if (s.logouts == 0) {
        size_t turn = 0;
        for (auto& accumulator : s.runtimes) {
          ++s.accumulations;
          self->send(accumulator, print_atom_v, self, s.iteration, turn);
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
          self->send(s.bench, complete_atom_v);

          if (s.last) {
            sample_stats stats(s.turn_series);
            std::vector<double> qos;

            for (size_t l = 0; l < s.finals.size(); ++l) {
              qos.push_back(sample_stats(s.finals.back()).stddev());
              s.finals.pop_back();
            }

            std::stringstream title_text;
            std::stringstream result_text;
            if (!parseable) {
              title_text << std::string(31, ' ') << std::setw(18) << "j-mean"
                         << std::setw(18) << "j-median" << std::setw(18)
                         << "j-error" << std::setw(18) << "j-stddev"
                         << std::setw(32) << "quality of service" << std::endl;
              result_text << "Turns" << std::string(27, ' ') << std::setw(17)
                          << stats.mean() << " " << std::setw(17)
                          << stats.median() << " " << std::setw(17)
                          << stats.err() << " " << std::setw(17)
                          << stats.stddev() << " " << std::setw(31)
                          << sample_stats(qos).median() << std::endl;
            } else {
              result_text << "Turns"
                          << "," << stats.mean() << "," << stats.median() << ","
                          << stats.err() << "," << stats.stddev() << ","
                          << sample_stats(qos).median();
            }

            const char* separator = parseable ? "," : ": ";
            std::stringstream act_text;
            if (!parseable)
              act_text << "\nActs:";
            act_text << "\nPost" << separator << s.actions[action::post]
                     << "\nLeave" << separator << s.actions[action::leave]
                     << "\nInvite" << separator << s.actions[action::invite]
                     << "\nCompute" << separator << s.actions[action::compute]
                     << "\nPostDelivery" << separator
                     << s.actions[action::post_delivery] << "\nIgnore"
                     << separator << s.actions[action::ignore] << "\nNone"
                     << separator << s.actions[action::none] << std::endl;

            self->send(s.bench, append_atom_v, title_text.str(),
                       result_text.str(), act_text.str());
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
  bool parseable = false;
  config() {
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
      .add(invite, "invite,i",
           "The invite behavior probability. Defaults to 10.")
      .add(befriend, "befriend,b", "The befriend probability. Defaults to 10.")
      .add(parseable, "parseable,P", "Print parseable output in CSV.");
  }
};

struct chatapp_state {
  const char* name = "chatapp";
};

caf::behavior
chatapp(caf::stateful_actor<chatapp_state>* self, const uint64_t clients,
        const uint64_t turns, const size_t directories, const uint64_t compute,
        const uint64_t post, const uint64_t leave, const uint64_t invite,
        const uint64_t befriend, bool parseable) {
  auto factory = behavior_factory(compute, post, leave, invite);
  auto poke_actor = self->spawn(poker, clients, turns, directories, befriend,
                                factory, parseable);
  return {
    [=](apply_atom, caf::actor& async_benchmark_completion, bool last) {
      self->send(poke_actor, apply_atom_v, async_benchmark_completion,
                 last);
    },
    [=](quit_atom) {
      self->send(poke_actor, quit_atom_v);
      self->quit();
    },
  };
}

void caf_main(caf::actor_system& system, const config& cfg) {
  if ((cfg.compute + cfg.post + cfg.leave + cfg.invite) != 100) {
    std::cerr << "Invalid arguments! Sum of probabilities != 100." << std::endl;
    return;
  } else if (cfg.clients < (cfg.directories * 2)) {
    std::cerr
      << "Invalid arguments! Clients are not at least twice the directories."
      << std::endl;
    return;
  } else if (cfg.befriend == 0 && cfg.invite > 0) {
    std::cerr << "Invalid arguments! Without a befriend chance all invites "
                 "will fail" << std::endl;
    return;
  } else {
    auto chat = system.spawn(chatapp, cfg.clients, cfg.turns, cfg.directories,
                             cfg.compute, cfg.post, cfg.leave, cfg.invite,
                             cfg.befriend, cfg.parseable);
    caf::scoped_actor self{system};
    std::vector<double> durations;
    auto perform_run = [&](bool collect_data) {
      auto start = std::chrono::high_resolution_clock::now();
      self->send(chat, apply_atom_v, self, collect_data);
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
      if (!cfg.parseable) {
        result_text << std::string(31, ' ') << std::setw(18) << "i-mean"
                    << std::setw(18) << "i-median" << std::setw(18) << "i-error"
                    << std::setw(18) << "i-stddev" << std::endl
                    << "Chat App" << std::string(24, ' ') << std::setw(17)
                    << stats.mean() << " " << std::setw(17) << stats.median()
                    << " " << std::setw(17) << stats.err() << " "
                    << std::setw(17) << stats.stddev() << std::endl;
      } else {
        result_text << "Chat App"
                    << "," << stats.mean() << "," << stats.median() << ","
                    << stats.err() << "," << stats.stddev() << std::endl;
      }
      std::cout << result_text.str();
      std::cout << title << result << act;
    });
    self->send(chat, quit_atom_v);
  }
}

CAF_MAIN(caf::id_block::chatapp)
