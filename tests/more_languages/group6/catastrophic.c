// c_test.c
#include <stdio.h>

struct Point
{
    int x;
    int y;
};

struct Point getOrigin()
{
    struct Point origin = {0, 0};
    return origin;
}

float mul_two_floats(float x1, float x2)
{
    return x1 * x2; // Use return to return a value
}

enum days
{
    SUN,
    MON,
    TUE,
    WED,
    THU,
    FRI,
    SAT
};

enum worker_pool_flags {
        /*
         * worker_pool flags
         *
         * A bound pool is either associated or disassociated with its CPU.
         * While associated (!DISASSOCIATED), all workers are bound to the
         * CPU and none has %WORKER_UNBOUND set and concurrency management
         * is in effect.
         *
         * While DISASSOCIATED, the cpu may be offline and all workers have
         * %WORKER_UNBOUND set and concurrency management disabled, and may
         * be executing on any CPU.  The pool behaves as an unbound one.
         *
         * Note that DISASSOCIATED should be flipped only while holding
         * wq_pool_attach_mutex to avoid changing binding state while
         * worker_attach_to_pool() is in progress.
         *
         * As there can only be one concurrent BH execution context per CPU, a
         * BH pool is per-CPU and always DISASSOCIATED.
         */
        POOL_BH                 = 1 << 0,
        POOL_MANAGER_ACTIVE     = 1 << 1,
        POOL_DISASSOCIATED      = 1 << 2,
        POOL_BH_DRAINING        = 1 << 3,
};

enum worker_flags {
        WORKER_DIE              = 1 << 1,
        WORKER_IDLE             = 1 << 2,
        WORKER_PREP             = 1 << 3,
        WORKER_CPU_INTENSIVE    = 1 << 6,
        WORKER_UNBOUND          = 1 << 7,
        WORKER_REBOUND          = 1 << 8,

        WORKER_NOT_RUNNING      = WORKER_PREP | WORKER_CPU_INTENSIVE |
                                  WORKER_UNBOUND | WORKER_REBOUND,
};


/*
 * Structure fields follow one of the following exclusion rules.
 *
 * I: Modifiable by initialization/destruction paths and read-only for
 *    everyone else.
 *
 * P: Preemption protected.  Disabling preemption is enough and should
 *    only be modified and accessed from the local cpu.
 *
 * L: pool->lock protected.  Access with pool->lock held.
 *
 * LN: pool->lock and wq_node_nr_active->lock protected for writes. Either for
 *     reads.
 *
 * K: Only modified by worker while holding pool->lock. Can be safely read by
 *    self, while holding pool->lock or from IRQ context if %current is the
 *    kworker.
 *
 * S: Only modified by worker self.
 *
 * A: wq_pool_attach_mutex protected.
 *
 * PL: wq_pool_mutex protected.
 *
 * PR: wq_pool_mutex protected for writes.  RCU protected for reads.
 *
 * PW: wq_pool_mutex and wq->mutex protected for writes.  Either for reads.
 *
 * PWR: wq_pool_mutex and wq->mutex protected for writes.  Either or
 *      RCU for reads.
 *
 * WQ: wq->mutex protected.
 *
 * WR: wq->mutex protected for writes.  RCU protected for reads.
 *
 * WO: wq->mutex protected for writes. Updated with WRITE_ONCE() and can be read
 *     with READ_ONCE() without locking.
 *
 * MD: wq_mayday_lock protected.
 *
 * WD: Used internally by the watchdog.
 */

/* struct worker is defined in workqueue_internal.h */

struct worker_pool {
	raw_spinlock_t		lock;		/* the pool lock */
	int			cpu;		/* I: the associated cpu */
	int			node;		/* I: the associated node ID */
	int			id;		/* I: pool ID */
	unsigned int		flags;		/* L: flags */

	unsigned long		watchdog_ts;	/* L: watchdog timestamp */
	bool			cpu_stall;	/* WD: stalled cpu bound pool */

	/*
	 * The counter is incremented in a process context on the associated CPU
	 * w/ preemption disabled, and decremented or reset in the same context
	 * but w/ pool->lock held. The readers grab pool->lock and are
	 * guaranteed to see if the counter reached zero.
	 */
	int			nr_running;

	struct list_head	worklist;	/* L: list of pending works */

	int			nr_workers;	/* L: total number of workers */
	int			nr_idle;	/* L: currently idle workers */

	struct list_head	idle_list;	/* L: list of idle workers */
	struct timer_list	idle_timer;	/* L: worker idle timeout */
	struct work_struct      idle_cull_work; /* L: worker idle cleanup */

	struct timer_list	mayday_timer;	  /* L: SOS timer for workers */

	/* a workers is either on busy_hash or idle_list, or the manager */
	DECLARE_HASHTABLE(busy_hash, BUSY_WORKER_HASH_ORDER);
						/* L: hash of busy workers */

	struct worker		*manager;	/* L: purely informational */
	struct list_head	workers;	/* A: attached workers */

	struct ida		worker_ida;	/* worker IDs for task name */

	struct workqueue_attrs	*attrs;		/* I: worker attributes */
	struct hlist_node	hash_node;	/* PL: unbound_pool_hash node */
	int			refcnt;		/* PL: refcnt for unbound pools */

	/*
	 * Destruction of pool is RCU protected to allow dereferences
	 * from get_work_pool().
	 */
	struct rcu_head		rcu;
};



long add_two_longs(long x1, long x2)
{
    return x1 + x2; // Use return to return a value
}

double multiplyByTwo(double num)
{
    return num * 2.0;
}

char getFirstCharacter(char *str)
{
    return str[0];
}

void greet(Person p)
{
    printf("Hello, %s\n", p.name);
}

typedef struct
{
    char name[50];
} Person;

typedef struct PersonA
{
    char name[50];
} PersonB;

int main()
{
    Person person;
    strcpy(person.name, "World");
    greet(person);
    return 0;
}

int* getArrayStart(int arr[], int size)
{
    return arr; // arr is equivalent to &arr[0]
}

long complexFunctionWithMultipleArguments(
    int param1,
    double param2,
    char *param3,
    struct Point point
) {
    // Some complex logic here
    long result = param1 + (long)param2 + param3[0] + point.x + point.y;
    return result;
}

// edge case examples from redis acl
/* Create a new key pattern. */
keyPattern *ACLKeyPatternCreate(sds pattern, int flags) {
    keyPattern *new = (keyPattern *) zmalloc(sizeof(keyPattern));
    new->pattern = pattern;
    new->flags = flags;
    return new;
}


/* Append the string representation of a key pattern onto the
 * provided base string. */
sds sdsCatPatternString(sds base, keyPattern *pat) {
    if (pat->flags == ACL_ALL_PERMISSION) {
        base = sdscatlen(base,"~",1);
    } else if (pat->flags == ACL_READ_PERMISSION) {
        base = sdscatlen(base,"%R~",3);
    } else if (pat->flags == ACL_WRITE_PERMISSION) {
        base = sdscatlen(base,"%W~",3);
    } else {
        serverPanic("Invalid key pattern flag detected");
    }
    return sdscatsds(base, pat->pattern);
}

/* Checks a channel against a provided list of channels. The is_pattern 
 * argument should only be used when subscribing (not when publishing)
 * and controls whether the input channel is evaluated as a channel pattern
 * (like in PSUBSCRIBE) or a plain channel name (like in SUBSCRIBE). 
 * 
 * Note that a plain channel name like in PUBLISH or SUBSCRIBE can be
 * matched against ACL channel patterns, but the pattern provided in PSUBSCRIBE
 * can only be matched as a literal against an ACL pattern (using plain string compare). */
static int ACLCheckChannelAgainstList(list *reference, const char *channel, int channellen, int is_pattern) {
    listIter li;
    listNode *ln;

    listRewind(reference, &li);
    while((ln = listNext(&li))) {
        sds pattern = listNodeValue(ln);
        size_t plen = sdslen(pattern);
        /* Channel patterns are matched literally against the channels in
         * the list. Regular channels perform pattern matching. */
        if ((is_pattern && !strcmp(pattern,channel)) || 
            (!is_pattern && stringmatchlen(pattern,plen,channel,channellen,0)))
        {
            return ACL_OK;
        }
    }
    return ACL_DENIED_CHANNEL;
}

// https://github.com/redis/redis/blob/9d0158bf89265daa96e1711478102147117f6b14/src/redis-benchmark.c#L81
static struct config {
    aeEventLoop *el;
    cliConnInfo conn_info;
    const char *hostsocket;
    int tls;
    struct cliSSLconfig sslconfig;
} config;

// cpp_test.cpp
#include <iostream>

class Person
{
    std::string name;

public:
    Person(std::string n) : name(n) {}
    void greet()
    {
        std::cout << "Hello, " << name << "\n";
    }
};

void globalGreet()
{
    std::cout << "Global greeting\n";
}

int main()
{
    Person person("World");
    person.greet();
    globalGreet();
    return 0;
}

#include <iostream>
#include <vector>
#include <map>
#include <string>
#include <functional>

// Simple global function
void printMessage(const std::string &message) {
    std::cout << message << std::endl;
}

// Template function
template<typename T>
void printVector(const std::vector<T>& vec) {
    for (const auto& item : vec) {
        std::cout << item << " ";
    }
    std::cout << std::endl;
}

// tricky example (thanks to X)
struct foo{
    char x;
    struct foo_in{
          char* y;
          short z;
    } inner;
};

// Struct example
struct Point {
    int x, y;
    Point(int x, int y) : x(x), y(y) {}
};

// Class example
class Animal {
  public:
    Animal(const std::string &name) : name(name) {}
    virtual void speak() const = 0;
    virtual ~Animal() = default;
protected:
    std::string name;
};

class Dog : public Animal {
  public:
    Dog(const std::string &name) : Animal(name) {}
    void speak() const override {
        std::cout << name << " says Woof!" << std::endl;
    }
};

class Cat : public Animal {
  public:
    Cat(const std::string &name) : Animal(name) {}
    void speak() const override {
        std::cout << name << " says Meow!" << std::endl;
    }
};

class CatDog: public Animal, public Cat, public Dog {
  public:
      CatDog(const std::string &name) : Animal(name) {}
      int meow_bark() {
        std::cout << name << " says Meow AND Bark!" << std::endl;
      }
};

nb::bytes BuildRnnDescriptor(int input_size, int hidden_size, int num_layers,
                             int batch_size, int max_seq_length, float dropout,
                             bool bidirectional, bool cudnn_allow_tf32,
			     int workspace_size, int reserve_space_size/* = {} */) {
  return PackDescriptor(RnnDescriptor{
      input_size, hidden_size, num_layers, batch_size, max_seq_length, dropout,
      bidirectional, cudnn_allow_tf32, workspace_size, reserve_space_size
  });
}

// Main function
int main() {
    printMessage("Hello, world!");

    std::vector<int> numbers = {1, 2, 3, 4, 5};
    printVector(numbers);

    Point p(10, 20);
    std::cout << "Point: (" << p.x << ", " << p.y << ")" << std::endl;

    Dog dog("Buddy");
    Cat cat("Whiskers");

    dog.speak();
    cat.speak();

    return 0;
}

// Enums are a way to assign a value to a constant most commonly used for
// easier visualization and reading of code
enum ECarTypes
{
  Sedan,
  Hatchback,
  SUV,
  Wagon
};

ECarTypes GetPreferredCarType()
{
    return ECarTypes::Hatchback;
}

enum ECarTypes : uint8_t
{
  Sedan, // 0
  Hatchback, // 1
  SUV = 254, // 254
  Hybrid // 255
};

// On the other hand you may not want enums to be accidentally cast to an integer
// type or to other enums so it is instead possible to create an enum class which
// won't be implicitly converted
enum class ECarTypes : uint8_t
{
  Sedan, // 0
  Hatchback, // 1
  SUV = 254, // 254
  Hybrid // 255
};

void myFunction(string fname, int age) {
  cout << fname << " Refsnes. " << age << " years old. \n";
}

// Always use std:: for <cmath> functions
template <typename T> T cos(T) = delete;
template <typename T> T sin(T) = delete;
template <typename T> T sqrt(T) = delete;

template<typename T> struct VLEN { static constexpr size_t val=1; };

// template class example from mlx/3rdparty/pocketfft.h
// borrowed here because it caused a parsing bug which
// included the entire file
/*
This file is part of pocketfft.

Copyright (C) 2010-2022 Max-Planck-Society
Copyright (C) 2019-2020 Peter Bell

For the odd-sized DCT-IV transforms:
  Copyright (C) 2003, 2007-14 Matteo Frigo
  Copyright (C) 2003, 2007-14 Massachusetts Institute of Technology

Authors: Martin Reinecke, Peter Bell

All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice, this
  list of conditions and the following disclaimer in the documentation and/or
  other materials provided with the distribution.
* Neither the name of the copyright holder nor the names of its contributors may
  be used to endorse or promote products derived from this software without
  specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
template<typename T> class arr
  {
  private:
    T *p;
    size_t sz;

#if defined(POCKETFFT_NO_VECTORS)
    static T *ralloc(size_t num)
      {
      if (num==0) return nullptr;
      void *res = malloc(num*sizeof(T));
      if (!res) throw std::bad_alloc();
      return reinterpret_cast<T *>(res);
      }
    static void dealloc(T *ptr)
      { free(ptr); }
#else
    static T *ralloc(size_t num)
      {
      if (num==0) return nullptr;
      void *ptr = aligned_alloc(64, num*sizeof(T));
      return static_cast<T*>(ptr);
      }
    static void dealloc(T *ptr)
      { aligned_dealloc(ptr); }
#endif

  public:
    arr() : p(0), sz(0) {}
    arr(size_t n) : p(ralloc(n)), sz(n) {}
    arr(arr &&other)
      : p(other.p), sz(other.sz)
      { other.p=nullptr; other.sz=0; }
    ~arr() { dealloc(p); }

    void resize(size_t n)
      {
      if (n==sz) return;
      dealloc(p);
      p = ralloc(n);
      sz = n;
      }

    T &operator[](size_t idx) { return p[idx]; }
    const T &operator[](size_t idx) const { return p[idx]; }

    T *data() { return p; }
    const T *data() const { return p; }

    size_t size() const { return sz; }
  };

// a couple of edge cases which led to issues, borrowed from MLX (c) Apple
class Buffer {
 private:
  void* ptr_;

std::tuple<array, array, array> quantize(
    const array& w,
    int group_size /* = 64 */,
    int bits /* = 4 */,
    StreamOrDevice s /* = {} */) {
  if (w.ndim() != 2) {
    throw std::invalid_argument("[quantize] Only matrices supported for now");
  }


/* Copyright 2021 The JAX Authors.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
==============================================================================*/

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include <stdint.h>

#if defined(__x86_64__) || defined(__amd64__) || defined(_M_IX86) || \
    defined(_M_X64)
#define PLATFORM_IS_X86
#endif

#if defined(_WIN32)
#define PLATFORM_WINDOWS
#endif

// SIMD extension querying is only available on x86.
#ifdef PLATFORM_IS_X86

#ifdef PLATFORM_WINDOWS
#if defined(_MSC_VER)
#include <intrin.h>
#endif

// Visual Studio defines a builtin function for CPUID, so use that if possible.
#define GETCPUID(a, b, c, d, a_inp, c_inp) \
  {                                        \
    int cpu_info[4] = {-1};                \
    __cpuidex(cpu_info, a_inp, c_inp);     \
    a = cpu_info[0];                       \
    b = cpu_info[1];                       \
    c = cpu_info[2];                       \
    d = cpu_info[3];                       \
  }

// Visual Studio defines a builtin function, so use that if possible.
static int GetXCR0EAX() { return _xgetbv(0); }

#else

// Otherwise use gcc-format assembler to implement the underlying instructions.
#define GETCPUID(a, b, c, d, a_inp, c_inp) \
  asm("mov %%rbx, %%rdi\n"                 \
      "cpuid\n"                            \
      "xchg %%rdi, %%rbx\n"                \
      : "=a"(a), "=D"(b), "=c"(c), "=d"(d) \
      : "a"(a_inp), "2"(c_inp))

static int GetXCR0EAX() {
  int eax, edx;
  asm("XGETBV" : "=a"(eax), "=d"(edx) : "c"(0));
  return eax;
}

#endif
#endif

// TODO(bionicles): technically we should use a proper parser
// and use configure-time tests instead of __AVX__, since there is a
// possibility that the compiler will use AVX instructions before we reach this
// point.
#ifdef PLATFORM_IS_X86

static void ReportMissingCpuFeature(const char* name) {
  PyErr_Format(
      PyExc_RuntimeError,
      "This version of jaxlib was built using %s instructions, which your "
      "CPU and/or operating system do not support. You may be able work around "
      "this issue by building jaxlib from source.", name);
}

static PyObject *CheckCpuFeatures(PyObject *self, PyObject *args) {
  uint32_t eax, ebx, ecx, edx;

  // To get general information and extended features we send eax = 1 and
  // ecx = 0 to cpuid.  The response is returned in eax, ebx, ecx and edx.
  // (See Intel 64 and IA-32 Architectures Software Developer's Manual
  // Volume 2A: Instruction Set Reference, A-M CPUID).
  GETCPUID(eax, ebx, ecx, edx, 1, 0);
  const uint64_t xcr0_xmm_mask = 0x2;
  const uint64_t xcr0_ymm_mask = 0x4;
  const uint64_t xcr0_avx_mask = xcr0_xmm_mask | xcr0_ymm_mask;
  const _Bool have_avx =
      // Does the OS support XGETBV instruction use by applications?
      ((ecx >> 27) & 0x1) &&
      // Does the OS save/restore XMM and YMM state?
      ((GetXCR0EAX() & xcr0_avx_mask) == xcr0_avx_mask) &&
      // Is AVX supported in hardware?
      ((ecx >> 28) & 0x1);
  const _Bool have_fma = have_avx && ((ecx >> 12) & 0x1);

  // Get standard level 7 structured extension features (issue CPUID with
  // eax = 7 and ecx= 0), which is required to check for AVX2 support as
  // well as other Haswell (and beyond) features.  (See Intel 64 and IA-32
  // Architectures Software Developer's Manual Volume 2A: Instruction Set
  // Reference, A-M CPUID).
  GETCPUID(eax, ebx, ecx, edx, 7, 0);
  const _Bool have_avx2 = have_avx && ((ebx >> 5) & 0x1);

#ifdef __AVX__
  if (!have_avx) {
    ReportMissingCpuFeature("AVX");
    return NULL;
  }
#endif  // __AVX__

#ifdef __AVX2__
  if (!have_avx2) {
    ReportMissingCpuFeature("AVX2");
    return NULL;
  }
#endif  // __AVX2__

#ifdef __FMA__
  if (!have_fma) {
    ReportMissingCpuFeature("FMA");
    return NULL;
  }
#endif  // __FMA__

  Py_INCREF(Py_None);
  return Py_None;
}

#else  // PLATFORM_IS_X86

static PyObject *CheckCpuFeatures(PyObject *self, PyObject *args) {
  Py_INCREF(Py_None);
  return Py_None;
}

#endif  // PLATFORM_IS_X86

static PyMethodDef cpu_feature_guard_methods[] = {
    {"check_cpu_features", CheckCpuFeatures, METH_NOARGS,
     "Throws an exception if the CPU is missing instructions used by jaxlib."},
    {NULL, NULL, 0, NULL}};

static struct PyModuleDef cpu_feature_guard_module = {
    PyModuleDef_HEAD_INIT, "cpu_feature_guard", /* name of module */
    NULL, -1, /* size of per-interpreter state of the module,
                 or -1 if the module keeps state in global variables. */
    cpu_feature_guard_methods};

#if defined(WIN32) || defined(_WIN32)
#define EXPORT_SYMBOL __declspec(dllexport)
#else
#define EXPORT_SYMBOL __attribute__ ((visibility("default")))
#endif

EXPORT_SYMBOL PyMODINIT_FUNC PyInit_cpu_feature_guard(void) {
  return PyModule_Create(&cpu_feature_guard_module);
}

typedef struct {
    GPT2Config config;
    // the weights (parameters) of the model, and their sizes
    ParameterTensors params;
    size_t param_sizes[NUM_PARAMETER_TENSORS];
    float* params_memory;
    size_t num_parameters;
    // gradients of the weights
    ParameterTensors grads;
    float* grads_memory;
    // buffers for the AdamW optimizer
    float* m_memory;
    float* v_memory;
    // the activations of the model, and their sizes
    ActivationTensors acts;
    size_t act_sizes[NUM_ACTIVATION_TENSORS];
    float* acts_memory;
    size_t num_activations;
    // gradients of the activations
    ActivationTensors grads_acts;
    float* grads_acts_memory;
    // other run state configuration
    int batch_size; // the batch size (B) of current forward pass
    int seq_len; // the sequence length (T) of current forward pass
    int* inputs; // the input tokens for the current forward pass
    int* targets; // the target tokens for the current forward pass
    float mean_loss; // after a forward pass with targets, will be populated with the mea
    n loss
} GPT2;