#pragma once

#include <functional>
#include <utility>
#include <algorithm>
#include <queue>

struct ordered_task_queue {
	typedef std::function<void()> task_t;

	ordered_task_queue(ordered_task_queue const&) = delete;
	ordered_task_queue& operator=(ordered_task_queue const &) & = delete;
	ordered_task_queue(ordered_task_queue&&) = default;
	ordered_task_queue& operator=(ordered_task_queue&&) & = default;

	ordered_task_queue() {
	}

	void push(const task_t& task) {
		tasks.push(task);
	}

	size_t run_one() {
		if (empty()) {
			return 0;
		}
		auto task = tasks.front();
		task();
		tasks.pop();
		return 1;
	}

	size_t run() {
		int cnt = 0;
		while (run_one() > 0) {
			cnt += 1;
		}
		return cnt;
	}

	bool empty() const {
		return tasks.empty();
	}

private:
	std::queue<task_t> tasks;
};