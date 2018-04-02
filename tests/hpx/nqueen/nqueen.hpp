//  Copyright (c) 2011 Vinay C Amatya
//  Copyright (c) 2011 Bryce Lelbach
//
//  Distributed under the Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)


#include <hpx/runtime.hpp>
#include <hpx/include/client.hpp>

#include <cstddef>
#include <utility>

namespace nqueen
{
    typedef std::vector<size_t> list_type;

    class board 
    {
    private:
        list_type list_;
        size_t count_;

    public:
        board() : list_(0), count_(0)
        {}

        board(list_type const& list, size_t, size_t)
          : list_(list), count_(0)
        {}

        ~board() = default;

        void init_board(size_t size)
        {
            for(size_t i=0; i<size; i++) {
                list_.push_back(size);
            }
        }

        bool check_board(list_type const& list, size_t level)
        {
            for (size_t i = 0; i < level; ++i) {
                if ((list.at(i) == list.at(level)) ||
                    (list.at(level) - list.at(i) == level - i) ||
                    (list.at(i) - list.at(level) == level - i))
                {
                    return false;
                }
            }
            return true;
        }

        list_type access_board()
        {
            return list_;
        }

        void update_board(size_t pos, size_t val)
        {
            list_.at(pos) = val;
        }

        void clear_board()
        {
            board::list_.clear();
        }

        size_t solve_board( list_type const& list, size_t size, size_t level, size_t col) 
        {
            board b(list, size, level);

            if (level == size) {
                return 1;
            } else if (level == 0) {
                b.update_board(level, col);
                if (b.check_board(b.access_board(), level)) {
                    b.count_ += solve_board(b.access_board(), size, level + 1, col);
                }
            } else {
                for (size_t i = 0; i < size; ++i) {
                    b.update_board(level, i);
                    if (b.check_board(b.access_board(), level)) {
                        b.count_ += solve_board(b.access_board(), size, level + 1, col);
                    }
                }
            }
            return b.count_;
        }
    };
}
