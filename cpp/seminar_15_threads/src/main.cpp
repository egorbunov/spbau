#include <iostream>
#include <thread>
#include <mutex>
#include <string>
#include <condition_variable>
#include <atomic>
#include <cstring>
#include <cassert>

struct evented_counter_t
{
    explicit evented_counter_t(size_t value)
    {
        (void)value;
    }
    evented_counter_t(const evented_counter_t &src) = delete;
    evented_counter_t(evented_counter_t &&src) = delete;
    virtual ~evented_counter_t() {}

    size_t get() const noexcept
    {
        return 0;
    }

    size_t inc() noexcept
    {
        return 0;
    }

    size_t dec() noexcept
    {
        return 0;
    }

    void wait_value(size_t target_value)
    {
        (void)target_value;
    }
};

struct write_info_t
{
    size_t offset;
    size_t size;
};

struct evented_buffer_t
{
    explicit evented_buffer_t(size_t size);
    evented_buffer_t(const evented_buffer_t &src) = delete;
    evented_buffer_t(evented_buffer_t &&src) = delete;
    virtual ~evented_buffer_t();

    size_t size() const;
    write_info_t wait_write();
    void write(size_t offset, size_t size, const char *src);
    void read(size_t offset, size_t size, char *dst);
};

evented_buffer_t::evented_buffer_t(size_t size)
{
    (void)size;
}

evented_buffer_t::~evented_buffer_t()
{
}

size_t evented_buffer_t::size() const
{
    return 0;
}

write_info_t evented_buffer_t::wait_write()
{
    write_info_t res;
    return res;
}

void evented_buffer_t::write(size_t offset,
        size_t size, const char *src)
{
    (void)offset;
    (void)size;
    (void)src;
}

void evented_buffer_t::read(size_t offset,
        size_t size, char *dst)
{
    (void)offset;
    (void)size;
    (void)dst;
}

std::atomic<bool> finish;
const char write_data[] = "0xDEADWRITE";
static std::mutex log_mutex;
#define NUM_READERS 3

static void reader_func_evented_counter(size_t id, evented_counter_t *evcnt)
{
    std::unique_lock<std::mutex> log_lock(
        log_mutex, std::defer_lock);
    size_t target_value = 10;
    while(!finish.load()) {
        evcnt->wait_value(target_value);
        target_value = target_value == 0 ? 10 : 0;

        log_lock.lock();
        std::cout << "[READER " << id << "]"
            << " Observed counter value " << evcnt->get()
            << std::endl;
        log_lock.unlock();
    }
}

static void writer_func_evented_counter(evented_counter_t *evcnt)
{
    std::unique_lock<std::mutex> log_lock(log_mutex, std::defer_lock);
    size_t steps_count = 0;
    bool up = true;
    while(steps_count < 100) {
        std::this_thread::sleep_for(std::chrono::milliseconds(50));
        if (up)
            evcnt->inc();
        else
            evcnt->dec();

        if (evcnt->get() == 10)
            up = false;
        else if (evcnt->get() == 0)
            up = true;

        log_lock.lock();
        std::cout << "[WRITER]"
            << " changed to " << evcnt->get()
            << std::endl;
        log_lock.unlock();

        ++steps_count;
    }
}

static void run_evented_counter()
{
    (void)writer_func_evented_counter;
    (void)reader_func_evented_counter;
}

static void reader_func_evented_buffer(size_t id, evented_buffer_t *evbuf)
{
    std::unique_lock<std::mutex> log_lock(
        log_mutex, std::defer_lock);
    char *data = new char[evbuf->size()];
    while(!finish.load()) {
        write_info_t wif = evbuf->wait_write();
        evbuf->read(wif.offset, wif.size, data);
        assert(!std::strcmp(data, write_data));

        log_lock.lock();
        std::cout << "[READER " << id << "]"
            << " Observed write to " << wif.offset
            << " of size " << wif.size
            << " " << std::string(data)
            << std::endl;
        log_lock.unlock();
    }
    delete[] data;
}

static void writer_func_evented_buffer(evented_buffer_t *evbuf)
{
    std::unique_lock<std::mutex> log_lock(
        log_mutex, std::defer_lock);
    const size_t write_size = sizeof(write_data);
    size_t write_pos = 0;
    while(write_pos + write_size < evbuf->size()) {
        std::this_thread::sleep_for(std::chrono::seconds(1));
        log_lock.lock();
        std::cout << "[WRITER]"
            << " Write to " << write_pos
            << " of size " << write_size
            << std::endl;
        log_lock.unlock();

        evbuf->write(write_pos, write_size, write_data);
        write_pos += write_size;
    }
}

// static void run_evented_buffer()
// {
//     (void)writer_func_evented_buffer;
//     (void)reader_func_evented_buffer;
// }

int main()
{
    run_evented_counter();
    // run_evented_buffer();
    return 0;
}
