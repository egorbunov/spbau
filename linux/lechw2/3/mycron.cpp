#include <stdlib.h>
#include <fstream>
#include <string>
#include <vector>
#include <iostream>
#include <unistd.h>
#include <ctime>

const std::string CONFIG_FILE = "/etc/mycron.cfg";
const std::string LOG_FILE = "/var/run/mycron/log.txt";

// Get current date/time, format is YYYY-MM-DD.HH:mm:ss
std::string currentDateTime() {
    time_t     now = time(0);
    struct tm  tstruct;
    char       buf[80];
    tstruct = *localtime(&now);
    // Visit http://en.cppreference.com/w/cpp/chrono/c/strftime
    // for more information about date/time format
    strftime(buf, sizeof(buf), "%Y-%m-%d.%X", &tstruct);
    return buf;
}

void log(std::string msg) {
  std::ofstream out(LOG_FILE, std::ofstream::out | std::ofstream::app);
  out << currentDateTime() << " | " << msg << std::endl;
  out.flush();
  out.close();
}

class Scheduler {
  struct Task {
    std::string command;
    size_t period; // in minutes
    size_t till_run;

    Task(std::string command, size_t period) : command(command), period(period), till_run(0) {
    }
  };

  const size_t SECONDS_IN_MINUTE = 3;

  std::vector<Task> tasks;
public:

  void addTask(std::string command, size_t period) {
    tasks.push_back(Task(command, period));
  }

  void doStep() {
  	log("INFO: looking for command to run, if it's time has come...");
    for (Task &task : tasks) {
      if (task.till_run == 0) {
        if (system(task.command.c_str()) < 0) {
        	log("ERROR: error occured during execution of command [" 
        		+ task.command + " ]");
        }
        task.till_run = task.period;
      }
      task.till_run -= 1;
    }
  }

  void run() {
    while (true) {
      doStep();
      sleep(SECONDS_IN_MINUTE);
    }
  }
};

int main() {
  using namespace std;

  bool isError = false;

  ifstream in(CONFIG_FILE, ifstream::in);
  Scheduler scheduler;
  string cmd, s_period;

  log("INFO: reading config. file from [ " + CONFIG_FILE + " ]");
  while (true) {
    if (!std::getline(in, s_period))
      break;
    int period = stoi(s_period);
    if (period < 0) {
      isError = true;
      log("ERROR: Bad configuration! Period cannot be negative");
    }

    if (!std::getline(in, cmd)) {
      isError = true;
      break;
    }

    scheduler.addTask(cmd, static_cast<size_t>(period));
  }
  in.close();

  if (isError) {
    return 1;
  }

  log("INFO: starting scheduler...");
  scheduler.run();

  return 0;
}