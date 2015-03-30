#include <iostream>
#include "loop_schedule.h"
//#include "hpx_runtime.h"
#include <thread>

extern boost::shared_ptr<hpx_runtime> hpx_backend;

using std::cout;
using std::endl;


//D is the signed version of T, for when T is unsigned
template<typename T, typename D=T>
void omp_static_init( int gtid, int schedtype, int *p_last_iter,
                      T *p_lower, T *p_upper,
                      D *p_stride, D incr, D chunk) {
    //auto loop_sched = &(hpx_backend->get_team()->loop_sched);
    //int team_size = loop_sched->num_threads;
    int team_size = hpx_backend->get_team()->num_threads;
    int trip_count = (*p_upper - *p_lower) / incr + 1;
    int adjustment = ((trip_count % team_size) == 0) ? -1 : 0;

    int block_size, stride, my_lower, my_upper;

    if (schedtype == kmp_sch_static) {
        *p_last_iter = ( gtid == trip_count - 1 );

        stride = (trip_count / team_size + adjustment + 1) * incr;
        block_size = (trip_count / team_size + adjustment) * incr;
        my_lower = *p_lower + gtid * stride;
        my_upper = my_lower + block_size;
        if( my_upper > *p_upper ) {
            my_upper = *p_upper;
        }
    } else { //kmp_sch_static_chunked
        cout << "executing chunked" << endl;
        block_size = (chunk - 1) * incr;
        stride = chunk * incr;
        my_lower = *p_lower + gtid * stride;
        my_upper = my_lower + block_size;
        *p_stride = stride * team_size;
        if(*p_last_iter) {
            *p_last_iter = (gtid == (trip_count - 1) / chunk) % team_size;
        }
    }
    *p_lower = my_lower;
    *p_upper = my_upper;
}

void
__kmpc_for_static_init_4( ident_t *loc, int32_t gtid, int32_t schedtype, 
                          int32_t *p_last_iter,int32_t *p_lower, int32_t *p_upper, 
                          int32_t *p_stride, int32_t incr, int32_t chunk ) 
{
    omp_static_init<int>( gtid, schedtype, p_last_iter, p_lower, p_upper,
                          p_stride, incr, chunk );
}

void
__kmpc_for_static_init_4u( ident_t *loc, int32_t gtid, int32_t schedtype,
                           int32_t *p_last_iter, uint32_t *p_lower, uint32_t *p_upper,
                           int32_t *p_stride, int32_t incr, int32_t chunk )
{
    omp_static_init<uint32_t, int>( gtid, schedtype, p_last_iter,
                                    p_lower, p_upper, p_stride, incr, chunk );
}

void
__kmpc_for_static_init_8( ident_t *loc, int32_t gtid, 
                          int32_t schedtype, int32_t *p_last_iter, 
                          int64_t *p_lower, int64_t *p_upper, 
                          int64_t *p_stride, int64_t incr, int64_t chunk ) 
{
    omp_static_init<int64_t>( gtid, schedtype, p_last_iter,
                               p_lower, p_upper, p_stride, incr, chunk );
}

void 
__kmpc_for_static_init_8u( ident_t *loc, int32_t gtid, 
                           int32_t schedtype, int32_t *p_last_iter, 
                           uint64_t *p_lower, uint64_t *p_upper,
                           int64_t *p_stride, int64_t incr, int64_t chunk )
{
    omp_static_init<uint64_t, int64_t>( gtid, schedtype, p_last_iter,
                                    p_lower, p_upper, p_stride, incr, chunk );
}

void
__kmpc_for_static_fini( ident_t *loc, int32_t gtid ){
    //Only seems to do internal tracking in intel runtime
}

//------------------------------------------------------------------------
//Dynamic loops:
//------------------------------------------------------------------------

//D is the signed version of T, for when T is unsigned
template<typename T, typename D=T>
void scheduler_init( int gtid, int schedtype, T lower, T upper, D stride, D chunk) {
    auto task = hpx_backend->get_task_data();
    auto team = hpx_backend->get_team();

    //TODO: look at the Intel code to see what data checks are done here. :738
    int NT = team->num_threads; //Is there ever a case where num_threads would be different than the number of threads in a current team?
    //if first in loop
    if(team->loop_list.size() <= task->loop_num) {
        team->loop_mtx.lock(); //making every thread wait here, until the struct is created.
        if(team->loop_list.size() == task->loop_num) {
            if( kmp_ord_lower & schedtype ) {
                //loop_sched->ordered = true;
                schedtype -= (kmp_ord_lower - kmp_sch_lower);
            }
            if( stride == 0 ) {
                stride = 1;
            }
            if( chunk == 0 ) { 
                chunk = 1;
            }
            team->loop_list.emplace_back( loop_data(NT, lower, upper, chunk, stride, schedtype) );
            //start trying to work with invalid data
        }
        team->loop_mtx.unlock();
    }

    team->loop_list[task->loop_num].first_iter[gtid] = 0;
    team->loop_list[task->loop_num].last_iter[gtid]  = 0;
    team->loop_list[task->loop_num].iter_count[gtid] = 0;
    task->loop_num++;
}


void 
__kmpc_dispatch_init_4( ident_t *loc, int32_t gtid, enum sched_type schedule,
                        int32_t lb, int32_t ub, int32_t st, int32_t chunk ) {
    scheduler_init<int32_t>( gtid, schedule, lb, ub, st, chunk);
}

void
__kmpc_dispatch_init_4u( ident_t *loc, int32_t gtid, enum sched_type schedule,
                         uint32_t lb, uint32_t ub, 
                         int32_t st, int32_t chunk ) {
    scheduler_init<uint32_t, int32_t>( gtid, schedule, lb, ub, st, chunk);
}

void
__kmpc_dispatch_init_8( ident_t *loc, int32_t gtid, enum sched_type schedule,
                        int64_t lb, int64_t ub, 
                        int64_t st, int64_t chunk ) {
    scheduler_init<int64_t>( gtid, schedule, lb, ub, st, chunk);
}

void
__kmpc_dispatch_init_8u( ident_t *loc, int32_t gtid, enum sched_type schedule,
                         uint64_t lb, uint64_t ub, 
                         int64_t st, int64_t chunk ) {
    scheduler_init<uint64_t, int64_t>( gtid, schedule, lb, ub, st, chunk);
}

//return one if there is work to be done, zero otherwise

template<typename T, typename D=T>
int kmp_next( int gtid, int *p_last, T *p_lower, T *p_upper, D *p_stride ) {
    //TODO p_last is not touched in this function
    //auto loop_sched = get_loop_sched();
    //auto loop_sched = &(hpx_backend->get_team()->loop_sched);
    int current_loop = hpx_backend->get_task_data()->loop_num - 1;
    auto loop_sched = &(hpx_backend->get_team()->loop_list[current_loop]);
    int schedule = loop_sched->schedule;
    T init;
    int loop_id;

    switch (schedule) {
        case kmp_sch_static_greedy:
        case kmp_sch_static:
        case kmp_ord_static:

            //I need to make sure a given thread gets work only once.
            if( loop_sched->iter_count[gtid] > 0 ) {
                //while( loop_sched->schedule_count < loop_sched->num_threads ) {
                //    loop_sched->yield();
                //}
                //loop_sched->num_workers--;
                return 0;
            } else {
                loop_sched->schedule_count++;
                //loop_sched->work_remains = (loop_sched->schedule_count < loop_sched->num_threads);
            }
            loop_sched->iter_count[gtid] = 1;

            *p_lower  = loop_sched->lower;
            *p_upper  = loop_sched->upper;
            *p_stride = loop_sched->stride;

            omp_static_init<T,D>( gtid, kmp_sch_static, p_last,
                                  p_lower, p_upper, p_stride, 
                                  loop_sched->stride, loop_sched->chunk);

            //if(loop_sched->ordered) {
                loop_sched->first_iter[gtid] = *p_lower / *p_stride ;
                loop_sched->last_iter[gtid] = *p_upper / *p_stride ;
            //}
            return 1;

        case kmp_sch_static_chunked: //1668
        case kmp_ord_static_chunked:

            loop_sched->schedule_count += loop_sched->chunk;
            loop_id = loop_sched->iter_count[gtid];

            loop_sched->first_iter[gtid] = gtid + loop_sched->iter_count[gtid] * loop_sched->stride * loop_sched->num_threads;
            loop_sched->last_iter[gtid] = loop_sched->first_iter[gtid] + loop_sched->stride;

            loop_sched->iter_count[gtid]++;

            *p_stride = loop_sched->stride;
            *p_lower  = loop_sched->lower + loop_sched->chunk * \
                        ( gtid + loop_sched->num_threads * (*p_stride) * loop_id);
            //FIXME: this logic is wrong
            *p_upper  = *p_lower + (*p_stride) * ( loop_sched->chunk - 1 );

            if(*p_upper > loop_sched->upper) {
                *p_upper = loop_sched->upper;//not sure this is 100% correct
            }

            if(*p_lower > loop_sched->upper) {
                //loop_sched->work_remains = ( loop_sched->schedule_count <= loop_sched->total_iter );
                ////need to keep the first thread from exiting before the last begins
                //while( loop_sched->work_remains ) { //this waits for all work is handed out. Can probably be done better.
                //    loop_sched->yield();
                //}
                //loop_sched->num_workers--;
                return 0;
            }
            //cout << "\tin next, gtid = " << gtid << " , lower = " << *p_lower << ", upper = " << *p_upper << endl;

            return 1;

        case kmp_sch_guided_chunked:
        case kmp_sch_dynamic_chunked:
        case kmp_ord_dynamic_chunked:
        case kmp_ord_guided_chunked:
        case kmp_sch_runtime:
        case kmp_ord_runtime:
            if((loop_sched->upper - loop_sched->lower) * loop_sched->stride < 0 ) {
                loop_sched->work_remains = false;
                //loop_sched->num_workers--;
            } else {
                *p_stride = loop_sched->stride;
                //*p_lower = loop_sched->lower;
                *p_lower = loop_sched->lower += ( (*p_stride) * loop_sched->chunk);
                *p_upper = *p_lower + (loop_sched->chunk - 1) * (*p_stride);
                //loop_sched->lower = *p_upper + *p_stride;
    
                loop_sched->first_iter[gtid] = loop_sched->schedule_count;
                loop_sched->last_iter[gtid] = loop_sched->first_iter[gtid] + loop_sched->chunk;
                loop_sched->schedule_count++;
            }

            return loop_sched->work_remains;

        default:
            if(gtid == 0) {
                cout << "default, scheduler = " << schedule << endl;
            }
    }
    return 0;
}

int
__kmpc_dispatch_next_4( ident_t *loc, int32_t gtid, int32_t *p_last,
                        int32_t *p_lb, int32_t *p_ub, int32_t *p_st ){
    return kmp_next<int32_t>(gtid, p_last, p_lb, p_ub, p_st);
}

int
__kmpc_dispatch_next_4u( ident_t *loc, int32_t gtid, int32_t *p_last,
                        uint32_t *p_lb, uint32_t *p_ub, int32_t *p_st ){
    return kmp_next<uint32_t, int32_t>(gtid, p_last, p_lb, p_ub, p_st);
}

int
__kmpc_dispatch_next_8( ident_t *loc, int32_t gtid, int32_t *p_last,
                        int64_t *p_lb, int64_t *p_ub, int64_t *p_st ){
    return kmp_next<int64_t>(gtid, p_last, p_lb, p_ub, p_st);
}

int
__kmpc_dispatch_next_8u( ident_t *loc, int32_t gtid, int32_t *p_last,
                        uint64_t *p_lb, uint64_t *p_ub, int64_t *p_st ){
    return kmp_next<uint64_t, int64_t>(gtid, p_last, p_lb, p_ub, p_st);
}


void __kmpc_dispatch_fini_4( ident_t *loc, kmp_int32 gtid ){
    //cout << "kmpc_dispatch_fini by thread " << gtid << endl;
}

void __kmpc_dispatch_fini_8( ident_t *loc, kmp_int32 gtid ){
}

void __kmpc_dispatch_fini_4u( ident_t *loc, kmp_int32 gtid ){
}

void __kmpc_dispatch_fini_8u( ident_t *loc, kmp_int32 gtid ){
}

void __kmpc_ordered(ident_t *, kmp_int32 global_tid ) {
    //auto loop_sched = &(hpx_backend->get_team()->loop_sched);
    int current_loop  = hpx_backend->get_task_data()->loop_num - 1;
    auto loop_sched = &(hpx_backend->get_team()->loop_list[ current_loop ]);
    while( loop_sched->ordered_count < loop_sched->first_iter[global_tid] ||
            loop_sched->ordered_count > loop_sched->last_iter[global_tid] ) {
        //cout << "\tThread " << global_tid << " waiting for ordered count to be " 
        //     << loop_sched->first_iter[global_tid] 
        //     << "(" << loop_sched->ordered_count << ")" << endl;
        loop_sched->yield();
    }
    //cout << "\tThread " << global_tid << " proceeding with ordered count = " 
    //     << loop_sched->ordered_count << "(" << loop_sched->first_iter[global_tid] 
    //     << ", " << loop_sched->last_iter[global_tid] << ")" << endl;
}

void __kmpc_end_ordered(ident_t *, kmp_int32 global_tid ) {
    //auto loop_sched = &(hpx_backend->get_team()->loop_sched);
    int current_loop  = hpx_backend->get_task_data()->loop_num - 1;
    auto loop_sched = &(hpx_backend->get_team()->loop_list[ current_loop ]);
    loop_sched->ordered_count++;
}
