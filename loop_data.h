#include <atomic>
#include <vector>

class loop_data {
    public:
        int lower;
        int upper;
        int stride;
        int chunk;
        std::atomic<int> loop_count{0}; //unused inside the loop
        std::atomic<int> num_workers{0}; 
        bool is_done = false;
        std::atomic<int> ordered_count{0};
        std::atomic<int> schedule_count{0};
        int num_threads;
        int schedule;
        std::vector<int> local_iter;
        std::vector<int> iter_remaining;
};

