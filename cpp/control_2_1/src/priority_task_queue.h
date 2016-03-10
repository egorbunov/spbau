#pragma once

#include <functional>
#include <queue>
#include <vector>
#include <utility>
#include <iterator>

template<class priority_t = size_t, class cmp_t = std::less<priority_t>>
struct priority_task_queue {
	typedef std::function<void()> task_t;

	priority_task_queue(priority_task_queue const&) = delete;
	priority_task_queue& operator=(priority_task_queue const &) & = delete;
	priority_task_queue(priority_task_queue&&) = default;
	priority_task_queue& operator=(priority_task_queue&&) & = default;

	priority_task_queue(const cmp_t& cmp = cmp_t()) : cmp(cmp) {
	}

	void push(const task_t& task, priority_t priority) {
		tasks.push(std::make_pair(task, priority));
	}

	size_t run_one() {
		if (empty()) {
			return 0;
		}
		auto task = tasks.top().first;
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
	typedef std::pair<task_t, priority_t> pair_t;
	struct priority_comp {
		bool operator()(pair_t a, pair_t b) {
			return cmp_t()(a.second, b.second);
		}
	};

	cmp_t cmp;
	std::priority_queue<pair_t, std::vector<pair_t>, priority_comp> tasks;
};

template<class fwd_it>
void sleep_sort(fwd_it begin, fwd_it end) {
	priority_task_queue<typename fwd_it::value_type, std::greater<typename fwd_it::value_type>> queue;

	auto head(begin);
	for (fwd_it it = begin; it != end; ++it) {
		auto val = *it;
		queue.push([&head, val]{ 
			*head = val;
			head++;
		}, 
			*it);
	}
	queue.run();
}