/* Copyright 2022 The TensorFlow Authors. All Rights Reserved.

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

#ifndef TENSORFLOW_CORE_CONFIG_FLAG_DEFS_H_
#define TENSORFLOW_CORE_CONFIG_FLAG_DEFS_H_

#include "tensorflow/core/config/flags.h"

namespace tensorflow {
namespace flags {

class Flags {
 public:
  // Test only flags. See flags_test.cc for example usage.
  TF_DECLARE_FLAG(test_only_experiment_1, true, "Test only experiment 1.");
  TF_DECLARE_FLAG(test_only_experiment_2, false, "Test only experiment 2.");

  // Declare flags below here.
  // LINT.IfChange
  TF_DECLARE_FLAG(enable_nested_function_shape_inference, false,
                  "Allow ops such as tf.cond to invoke the ShapeRefiner on "
                  "their nested functions.");
  TF_DECLARE_FLAG(enable_quantized_dtypes_training, false,
                  "Set quantized dtypes, like tf.qint8, to be trainable.");
  TF_DECLARE_FLAG(graph_building_optimization, false,
                  "Optimize graph building for faster tf.function tracing.");
  TF_DECLARE_FLAG(
      op_building_optimization, true,
      "Optimize tf.Operation building for faster tf.function tracing.");
  TF_DECLARE_FLAG(saved_model_fingerprinting, true,
                  "Add fingerprint to SavedModels.");
  TF_DECLARE_FLAG(
      tf_shape_default_int64, false,
      "The default output of tf.shape (i.e. when out_type is not specified) is "
      "int64 when this flag is true and int32 otherwise. Setting this to true "
      "is an unsupported, experimental setting that causes known breakages.");
  TF_DECLARE_FLAG(more_stack_traces, false,
                  "Enable experimental code that preserves and propagates "
                  "graph node stack traces in C++.");
  TF_DECLARE_FLAG(publish_function_graphs, true,
                  "Enables the publication of partitioned function graphs "
                  "via StatsPublisherInterface. Disabling this flag can "
                  "reduce memory consumption.");
  TF_DECLARE_FLAG(enable_aggressive_constant_replication, true,
                  "Replicate constants across CPU devices and even for local "
                  "CPUs within the same task if available.")
  TF_DECLARE_FLAG(enable_colocation_key_propagation_in_while_op_lowering, false,
                  "If true, colocation key attributes for the ops will be "
                  "propagated during while op lowering to switch/merge ops.")
  // LINT.ThenChange(//tensorflow/core/config/flags_api_wrapper.cc)
};

Flags& Global();

}  // namespace flags
}  // namespace tensorflow

#endif  // TENSORFLOW_CORE_CONFIG_FLAG_DEFS_H_

/* Copyright 2018 The TensorFlow Authors. All Rights Reserved.

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

#include "tensorflow/compiler/jit/flags.h"

#include <limits>
#include <mutex>  // NOLINT
#include <optional>
#include <vector>

#include "absl/base/call_once.h"
#include "absl/strings/numbers.h"
#include "absl/strings/str_split.h"
#include "absl/strings/strip.h"
#include "tensorflow/compiler/mlir/tensorflow/utils/dump_graph.h"
#include "xla/parse_flags_from_env.h"
#include "tensorflow/core/platform/macros.h"
#include "tensorflow/core/tpu/kernels/sparse_core_xla_flags_defaults.h"
#include "tensorflow/core/util/command_line_flags.h"

namespace tensorflow {
namespace {

BuildXlaOpsPassFlags* build_ops_flags;
MarkForCompilationPassFlags* mark_for_compilation_flags;
XlaDeviceFlags* device_flags;
XlaSparseCoreFlags* sparse_core_flags;
XlaOpsCommonFlags* ops_flags;
XlaCallModuleFlags* call_module_flags;
MlirCommonFlags* mlir_flags;
JitRtFlags* jitrt_flags;
std::vector<Flag>* jitrt_flag_list;

std::vector<Flag>* flag_list;
absl::once_flag flags_init;

bool SetterForXlaAutoJitFlag(const string& value) {
  int32_t opt_level;
  // We need to use the mark_for_compilation_flags directly here instead of
  // going via GetMarkForCompilationPassFlags() to avoid infinite recursion. The
  // latter will try to setup and parse flags, which would bring us back to this
  // setter.
  if (absl::SimpleAtoi(value, &opt_level)) {
    mark_for_compilation_flags->xla_auto_jit_flag
        .optimization_level_single_gpu = opt_level;
    mark_for_compilation_flags->xla_auto_jit_flag.optimization_level_general =
        opt_level;
    return true;
  }

  if (value == "fusible") {
    mark_for_compilation_flags->xla_auto_jit_flag
        .optimization_level_single_gpu = 1;
    mark_for_compilation_flags->xla_auto_jit_flag.optimization_level_general =
        1;
    mark_for_compilation_flags->tf_xla_ops_to_cluster = "FUSIBLE";
    return true;
  }

  absl::string_view value_sv(value);
  if (!absl::ConsumePrefix(&value_sv, "single-gpu(") ||
      !absl::ConsumeSuffix(&value_sv, ")") ||
      !absl::SimpleAtoi(value_sv, &opt_level)) {
    return false;
  }

  mark_for_compilation_flags->xla_auto_jit_flag.optimization_level_single_gpu =
      opt_level;
  return true;
}

bool SetterForXlaCallModuleDisabledChecks(const string& value) {
  auto directives = absl::StrSplit(value, ',', absl::SkipEmpty());
  call_module_flags->disabled_checks.insert(directives.begin(),
                                            directives.end());
  return true;
}

void AppendMarkForCompilationPassFlagsInternal(std::vector<Flag>* flag_list) {
  std::vector<Flag> new_flags = {
      Flag("tf_xla_auto_jit", SetterForXlaAutoJitFlag, "0",
           "Control compilation of operators into XLA computations on CPU and "
           "GPU devices.  0 = use ConfigProto setting; -1 = off; 1 = on for "
           "things very likely to be improved; 2 = on for everything; "
           "(experimental) fusible = only for Tensorflow operations that XLA "
           "knows how to fuse.  "
           "If set to single-gpu(<N>) then this resolves to <N> for single-GPU "
           "graphs (graphs that have at least one node placed on a GPU and no "
           "more than one GPU is in use through the entire graph) and 0 "
           "otherwise.  Experimental."),
      Flag("tf_xla_min_cluster_size",
           &mark_for_compilation_flags->tf_xla_min_cluster_size,
           "Minimum number of operators in an XLA compilation. Ignored for "
           "operators placed on an XLA device or operators explicitly marked "
           "for compilation."),
      Flag("tf_xla_max_cluster_size",
           &mark_for_compilation_flags->tf_xla_max_cluster_size,
           "Maximum number of operators in an XLA compilation."),
      Flag(
          "tf_xla_ops_to_cluster",
          &mark_for_compilation_flags->tf_xla_ops_to_cluster,
          "(experimental) "
          "Limit the operations clustered by XLA to these operations. "
          "If multiple, separate them with commas. Shortcuts: "
          " PW: All point-wise operations."
          " RED: All reduction operations."
          " MISC: Mixed operations."
          " PWRED: TF operations that get converted to PW+RED operation in XLA."
          " REDUCEWINDOW: TF operations like MaxPool/AvgPool that get "
          "converted to ReduceWindow in XLA."
          " REDUCEWINDOWPW: Operation that get converted to ReduceWindow + PW "
          "(LRN, LRNGrad)."
          " BN: TF FusedBatchNorm* operations."
          " FUSIBLE: All TF operations that XLA can fuse (All the above). "
          "You can also put any TF operation name, e.g. 'FUSIBLE,MatMul'."),
      Flag("tf_xla_cluster_exclude_ops",
           &mark_for_compilation_flags->tf_xla_cluster_exclude_ops,
           "(experimental) "
           "Exclude the operations from auto-clustering. "
           "If multiple, separate them with commas."
           " Where, Some_other_ops"),
      Flag("tf_xla_clustering_debug",
           &mark_for_compilation_flags->tf_xla_clustering_debug,
           "Dump graphs during XLA compilation."),
      Flag("tf_xla_cpu_global_jit",
           &mark_for_compilation_flags->tf_xla_cpu_global_jit,
           "Enables global JIT compilation for CPU via SessionOptions."),
      Flag("tf_xla_clustering_fuel",
           &mark_for_compilation_flags->tf_xla_clustering_fuel,
           "Places an artificial limit on the number of ops marked as "
           "eligible for clustering."),
      Flag("tf_xla_disable_deadness_safety_checks_for_debugging",
           &mark_for_compilation_flags
                ->tf_xla_disable_deadness_safety_checks_for_debugging,
           "Disable deadness related safety checks when clustering (this is "
           "unsound)."),
      Flag("tf_xla_disable_resource_variable_safety_checks_for_debugging",
           &mark_for_compilation_flags
                ->tf_xla_disable_resource_variable_safety_checks_for_debugging,
           "Disable resource variables related safety checks when clustering "
           "(this is unsound)."),
      Flag("tf_xla_deterministic_cluster_names",
           &mark_for_compilation_flags->tf_xla_deterministic_cluster_names,
           "Causes the function names assigned by auto clustering to be "
           "deterministic from run to run."),
      Flag("tf_xla_persistent_cache_directory",
           &mark_for_compilation_flags->tf_xla_persistent_cache_directory,
           "If non-empty, JIT-compiled executables are saved to and loaded "
           "from the specified file system directory path. Empty by default."),
      Flag("tf_xla_persistent_cache_device_types",
           &mark_for_compilation_flags->tf_xla_persistent_cache_device_types,
           "If non-empty, the persistent cache will only be used for the "
           "specified devices (comma separated). Each device type should be "
           "able to be converted to `DeviceType`."),
      Flag("tf_xla_persistent_cache_read_only",
           &mark_for_compilation_flags->tf_xla_persistent_cache_read_only,
           "If true, the persistent cache will be read-only."),
      Flag("tf_xla_disable_strict_signature_checks",
           &mark_for_compilation_flags->tf_xla_disable_strict_signature_checks,
           "If true, entires loaded into the XLA compile cache will not have "
           "their signatures checked strictly. Defaults to false."),
      Flag("tf_xla_persistent_cache_prefix",
           &mark_for_compilation_flags->tf_xla_persistent_cache_prefix,
           "Specifies the persistance cache prefix. Default is "
           "\"xla_compile_cache\""),
      Flag("tf_xla_sparse_core_disable_table_stacking",
           &sparse_core_flags->tf_xla_sparse_core_disable_table_stacking,
           "Disable table stacking for all the tables passed to the SparseCore"
           "mid level API."),
      Flag("tf_xla_sparse_core_minibatch_max_division_level",
           &sparse_core_flags->tf_xla_sparse_core_minibatch_max_division_level,
           "Max level of division to split input data into minibatches."),
      Flag("tf_xla_sparse_core_stacking_mem_limit_bytes",
           &sparse_core_flags->tf_xla_sparse_core_stacking_mem_limit_bytes,
           "If non-zero, limits the size of the activations for a given table"
           "to be below these many bytes."),
      Flag("tf_xla_sparse_core_stacking_table_shard_limit_bytes",
           &sparse_core_flags
                ->tf_xla_sparse_core_stacking_table_shard_limit_bytes,
           "If non-zero, limits the size of any table shard to be below these"
           "many bytes.")};
  flag_list->insert(flag_list->end(), new_flags.begin(), new_flags.end());
}

void AllocateAndParseJitRtFlags() {
  jitrt_flags = new JitRtFlags;
  jitrt_flags->always_specialize = false;
  jitrt_flags->cost_driven_async_parallel_for = false;
  jitrt_flags->enable_crash_reproducer = false;
  jitrt_flags->log_query_of_death = false;
  jitrt_flags->vectorize = false;
  jitrt_flag_list = new std::vector<Flag>({
      Flag("always_specialize", &jitrt_flags->always_specialize, ""),
      Flag("cost_driven_async_parallel_for",
           &jitrt_flags->cost_driven_async_parallel_for, ""),
      Flag("enable_crash_reproducer", &jitrt_flags->enable_crash_reproducer,
           ""),
      Flag("log_query_of_death", &jitrt_flags->log_query_of_death, ""),
      Flag("vectorize", &jitrt_flags->vectorize, ""),
  });
  xla::ParseFlagsFromEnvAndDieIfUnknown("TF_JITRT_FLAGS", *jitrt_flag_list);
}

void AllocateAndParseFlags() {
  build_ops_flags = new BuildXlaOpsPassFlags;
  build_ops_flags->tf_xla_enable_lazy_compilation = true;
  build_ops_flags->tf_xla_print_cluster_outputs = false;
  build_ops_flags->tf_xla_check_cluster_input_numerics = false;
  build_ops_flags->tf_xla_check_cluster_output_numerics = false;
  build_ops_flags->tf_xla_disable_constant_folding = false;
  build_ops_flags->tf_xla_disable_full_embedding_pipelining = false;
  build_ops_flags->tf_xla_embedding_parallel_iterations = 0;

  mark_for_compilation_flags = new MarkForCompilationPassFlags;
  mark_for_compilation_flags->xla_auto_jit_flag.optimization_level_single_gpu =
      0;
  mark_for_compilation_flags->xla_auto_jit_flag.optimization_level_general = 0;
  mark_for_compilation_flags->tf_xla_min_cluster_size = 4;
  mark_for_compilation_flags->tf_xla_max_cluster_size =
      std::numeric_limits<int32>::max();
  mark_for_compilation_flags->tf_xla_clustering_debug = false;
  mark_for_compilation_flags->tf_xla_cpu_global_jit = false;
  mark_for_compilation_flags->tf_xla_clustering_fuel =
      std::numeric_limits<int64_t>::max();
  mark_for_compilation_flags
      ->tf_xla_disable_deadness_safety_checks_for_debugging = false;
  mark_for_compilation_flags
      ->tf_xla_disable_resource_variable_safety_checks_for_debugging = false;
  mark_for_compilation_flags->tf_xla_deterministic_cluster_names = false;
  mark_for_compilation_flags->tf_xla_persistent_cache_directory = "";
  mark_for_compilation_flags->tf_xla_persistent_cache_device_types = "";
  mark_for_compilation_flags->tf_xla_persistent_cache_read_only = false;
  mark_for_compilation_flags->tf_xla_disable_strict_signature_checks = false;
  mark_for_compilation_flags->tf_xla_persistent_cache_prefix =
      "xla_compile_cache";

  device_flags = new XlaDeviceFlags;
  device_flags->tf_xla_compile_on_demand = false;
  device_flags->tf_xla_enable_xla_devices = false;

  sparse_core_flags = new XlaSparseCoreFlags;
  sparse_core_flags->tf_xla_sparse_core_minibatch_max_division_level =
      kDefaultSparseCoreMinibatchMaxDivisionLevel;
  sparse_core_flags->tf_xla_sparse_core_disable_table_stacking =
      kDefaultDisableTableStacking;
  sparse_core_flags->tf_xla_sparse_core_stacking_mem_limit_bytes =
      kDefaultXlaSparseCoreStackingMemLimit;
  sparse_core_flags->tf_xla_sparse_core_stacking_table_shard_limit_bytes =
      kDefaultXlaSparseCoreStackingTableShardLimit;

  ops_flags = new XlaOpsCommonFlags;
  ops_flags->tf_xla_always_defer_compilation = false;
  ops_flags->tf_xla_async_compilation = false;
  ops_flags->tf_xla_use_device_api.enabled_for_xla_launch_ = true;
  ops_flags->tf_xla_use_device_api.enabled_for_compile_on_demand_ = true;
  ops_flags->tf_xla_use_device_api.enabled_for_compile_and_run_ = true;
  ops_flags->tf_xla_use_device_api.enabled_for_all_ = false;
  ops_flags->tf_xla_use_device_api.enabled_for_gpu_ = true;

  call_module_flags = new XlaCallModuleFlags;
  // The `enable_mlir_bridge` flag allows the user to explicitly request that
  // their program is (or isn't) compiled using the MLIR-based TF-to-XLA bridge.
  //
  // The `enable_mlir_bridge_is_explicit` variable tracks whether or not the
  // user has made an explicit request. That is, if this variable is set to
  // true, the program honors the user's request as per `enable_mlir_bridge`; if
  // it's set to false, the default behavior is used (which may run either
  // bridge, on a per-graph basis).
  bool enable_mlir_bridge = false;
  bool enable_mlir_bridge_is_explicit = false;
  bool enable_mlir_merge_control_flow_pass = true;
  bool enable_mlir_convert_control_to_data_outputs_pass = false;
  bool enable_mlir_strict_clusters = false;
  bool enable_mlir_multiple_local_cpu_devices = false;
  // Dump graphs in TFG dialect.
  bool use_tfg_graph_dumper = false;
  bool enable_mlir_generic_outside_compilation = false;
  bool enable_tpu_variable_runtime_reformatting_pass = true;

  flag_list = new std::vector<Flag>(
      {Flag("tf_xla_enable_lazy_compilation",
            &build_ops_flags->tf_xla_enable_lazy_compilation, ""),
       Flag("tf_xla_print_cluster_outputs",
            &build_ops_flags->tf_xla_print_cluster_outputs,
            "If true then insert Print nodes to print out values produced by "
            "XLA clusters."),
       Flag("tf_xla_check_cluster_input_numerics",
            &build_ops_flags->tf_xla_check_cluster_input_numerics,
            "If true then insert CheckNumerics nodes to check all cluster "
            "inputs."),
       Flag("tf_xla_check_cluster_output_numerics",
            &build_ops_flags->tf_xla_check_cluster_output_numerics,
            "If true then insert CheckNumerics nodes to check all cluster "
            "outputs."),
       Flag("tf_xla_disable_constant_folding",
            &build_ops_flags->tf_xla_disable_constant_folding,
            "If true then disables constant folding on TF graph before XLA "
            "compilation."),
       Flag("tf_xla_disable_full_embedding_pipelining",
            &build_ops_flags->tf_xla_disable_full_embedding_pipelining,
            "If true then disables full embedding pipelining and instead use "
            "strict SparseCore / TensorCore sequencing."),
       Flag("tf_xla_embedding_parallel_iterations",
            &build_ops_flags->tf_xla_embedding_parallel_iterations,
            "If >0 then use this many parallel iterations in "
            "embedding_pipelining and embedding_sequency. By default, use the "
            "parallel_iterations on the original model WhileOp."),

       Flag("tf_xla_compile_on_demand", &device_flags->tf_xla_compile_on_demand,
            "Switch a device into 'on-demand' mode, where instead of "
            "autoclustering ops are compiled one by one just-in-time."),

       Flag("tf_xla_enable_xla_devices",
            &device_flags->tf_xla_enable_xla_devices,
            "Generate XLA_* devices, where placing a computation on such a "
            "device"
            "forces compilation by XLA. Deprecated."),

       Flag("tf_xla_always_defer_compilation",
            &ops_flags->tf_xla_always_defer_compilation, ""),
       Flag("tf_xla_async_compilation", &ops_flags->tf_xla_async_compilation,
            "When lazy compilation is enabled, asynchronous compilation starts "
            "the cluster compilation in the background, and the fallback path "
            "is executed until the compilation has finished."),
       Flag("tf_xla_use_device_api_for_xla_launch",
            &ops_flags->tf_xla_use_device_api.enabled_for_xla_launch_,
            "If true, uses Device API (PjRt) for single device compilation and "
            "execution of functions marked for JIT compilation i.e. "
            "jit_compile=True. Defaults to false."),
       Flag("tf_xla_use_device_api_for_compile_on_demand",
            &ops_flags->tf_xla_use_device_api.enabled_for_compile_on_demand_,
            "If true, uses Device API (PjRt) for compiling and executing ops "
            "one by one in 'on-demand' mode. Defaults to false."),
       Flag("tf_xla_use_device_api_for_auto_jit",
            &ops_flags->tf_xla_use_device_api.enabled_for_compile_and_run_,
            "If true, uses Device API (PjRt) for compilation and execution "
            "when auto-clustering is enabled. Defaults to false."),
       Flag("tf_xla_use_device_api",
            &ops_flags->tf_xla_use_device_api.enabled_for_all_,
            "If true, uses Device API (PjRt) for compilation and execution "
            "of ops one-by-one in 'on-demand' mode, for functions marked for "
            "JIT compilation, or when auto-clustering is enabled. Defaults to "
            "false."),
       Flag("tf_xla_enable_device_api_for_gpu",
            &ops_flags->tf_xla_use_device_api.enabled_for_gpu_,
            "If true, uses Device API (PjRt) for TF GPU device. This is a "
            "helper flag so that individual tests can turn on PjRt for GPU "
            "specifically."),

       Flag("tf_xla_call_module_disabled_checks",
            SetterForXlaCallModuleDisabledChecks, "",
            "A comma-sepated list of directives specifying the safety checks "
            "to be skipped when compiling XlaCallModuleOp. See the op "
            "documentation for the recognized values."),

       Flag("tf_mlir_enable_mlir_bridge", &enable_mlir_bridge,
            "Enables experimental MLIR-Based TensorFlow Compiler Bridge.",
            &enable_mlir_bridge_is_explicit),
       Flag("tf_mlir_enable_merge_control_flow_pass",
            &enable_mlir_merge_control_flow_pass,
            "Enables MergeControlFlow pass for MLIR-Based TensorFlow Compiler "
            "Bridge."),
       Flag("tf_mlir_enable_convert_control_to_data_outputs_pass",
            &enable_mlir_convert_control_to_data_outputs_pass,
            "Enables `tf-executor-convert-control-to-data-outputs` pass for "
            "MLIR-Based TensorFlow Compiler Bridge."),
       Flag("tf_mlir_enable_strict_clusters", &enable_mlir_strict_clusters,
            "Do not allow clusters that have cyclic control dependencies."),
       Flag("tf_mlir_enable_multiple_local_cpu_devices",
            &enable_mlir_multiple_local_cpu_devices,
            "Enable multiple local CPU devices. CPU ops which are outside "
            "compiled inside the tpu cluster will also be replicated across "
            "multiple cpu devices."),
       Flag("tf_dump_graphs_in_tfg", &use_tfg_graph_dumper,
            "When tf_dump_graphs_in_tfg is true, graphs after transformations "
            "are dumped in MLIR TFG dialect and not in GraphDef"),
       Flag("tf_mlir_enable_generic_outside_compilation",
            &enable_mlir_generic_outside_compilation,
            "Enables OutsideCompilation passes for MLIR-Based TensorFlow "
            "Generic Compiler Bridge."),
       Flag("tf_mlir_enable_tpu_variable_runtime_reformatting_pass",
            &enable_tpu_variable_runtime_reformatting_pass,
            "Enables TPUVariableRuntimeReformatting pass for MLIR-Based "
            "TensorFlow Compiler Bridge. This enables weight update sharding "
            "and creates TPUReshardVariables ops.")});

  AppendMarkForCompilationPassFlagsInternal(flag_list);
  xla::ParseFlagsFromEnvAndDieIfUnknown("TF_XLA_FLAGS", *flag_list);

  mlir_flags = new MlirCommonFlags;
  if (!enable_mlir_bridge_is_explicit) {
    mlir_flags->tf_mlir_enable_mlir_bridge =
        ConfigProto::Experimental::MLIR_BRIDGE_ROLLOUT_UNSPECIFIED;
  } else if (enable_mlir_bridge) {
    mlir_flags->tf_mlir_enable_mlir_bridge =
        ConfigProto::Experimental::MLIR_BRIDGE_ROLLOUT_ENABLED;
  } else {
    mlir_flags->tf_mlir_enable_mlir_bridge =
        ConfigProto::Experimental::MLIR_BRIDGE_ROLLOUT_DISABLED;
  }
  mlir_flags->tf_mlir_enable_merge_control_flow_pass =
      enable_mlir_merge_control_flow_pass;
  mlir_flags->tf_mlir_enable_convert_control_to_data_outputs_pass =
      enable_mlir_convert_control_to_data_outputs_pass;
  mlir_flags->tf_mlir_enable_strict_clusters = enable_mlir_strict_clusters;
  mlir_flags->tf_mlir_enable_generic_outside_compilation =
      enable_mlir_generic_outside_compilation;
  mlir_flags->tf_mlir_enable_tpu_variable_runtime_reformatting_pass =
      enable_tpu_variable_runtime_reformatting_pass;
  mlir_flags->tf_mlir_enable_multiple_local_cpu_devices =
      enable_mlir_multiple_local_cpu_devices;

  if (use_tfg_graph_dumper) {
    UseMlirForGraphDump(MlirDumpConfig{}.elide_large_attributes().emit_dialect(
        MlirDumpConfig::Dialect::kTFG));
  }

  AllocateAndParseJitRtFlags();
}

void ResetFlags() {
  delete build_ops_flags;
  delete mark_for_compilation_flags;
  delete device_flags;
  delete ops_flags;
  delete mlir_flags;
  delete flag_list;
  delete jitrt_flags;
  delete jitrt_flag_list;
  AllocateAndParseFlags();
}

}  // namespace

bool SetXlaAutoJitFlagFromFlagString(const string& value) {
  absl::call_once(flags_init, &AllocateAndParseFlags);
  return SetterForXlaAutoJitFlag(value);
}

BuildXlaOpsPassFlags* GetBuildXlaOpsPassFlags() {
  absl::call_once(flags_init, &AllocateAndParseFlags);
  return build_ops_flags;
}

MarkForCompilationPassFlags* GetMarkForCompilationPassFlags() {
  absl::call_once(flags_init, &AllocateAndParseFlags);
  return mark_for_compilation_flags;
}

XlaSparseCoreFlags* GetXlaSparseCoreFlags() {
  absl::call_once(flags_init, &AllocateAndParseFlags);
  return sparse_core_flags;
}

XlaDeviceFlags* GetXlaDeviceFlags() {
  absl::call_once(flags_init, &AllocateAndParseFlags);
  return device_flags;
}

XlaOpsCommonFlags* GetXlaOpsCommonFlags() {
  absl::call_once(flags_init, &AllocateAndParseFlags);
  return ops_flags;
}

XlaCallModuleFlags* GetXlaCallModuleFlags() {
  absl::call_once(flags_init, &AllocateAndParseFlags);
  return call_module_flags;
}

MlirCommonFlags* GetMlirCommonFlags() {
  absl::call_once(flags_init, &AllocateAndParseFlags);
  return mlir_flags;
}

void ResetJitCompilerFlags() { ResetFlags(); }

const JitRtFlags& GetJitRtFlags() {
  absl::call_once(flags_init, &AllocateAndParseFlags);
  return *jitrt_flags;
}

ConfigProto::Experimental::MlirBridgeRollout GetMlirBridgeRolloutState(
    std::optional<const ConfigProto> config_proto) {
  // TF1 graphs that do not override Sessions's ConfigProto and TF2 graphs
  // can enable/disable the graph via tf_mlir_enable_mlir_bridge.
  auto tf_mlir_enable_mlir_bridge =
      GetMlirCommonFlags()->tf_mlir_enable_mlir_bridge;
  if (tf_mlir_enable_mlir_bridge !=
      ConfigProto::Experimental::MLIR_BRIDGE_ROLLOUT_UNSPECIFIED) {
    return tf_mlir_enable_mlir_bridge;
  }

  // If a ConfigProto was not passed in, we can assume the caller is
  // checking if TF2 graph should have the bridge enabled / disabled. In that
  // case, we have already checked tf_mlir_enable_mlir_bridge so it is safe to
  // return UNSPECIFIED here.
  if (!config_proto.has_value()) {
    return ConfigProto::Experimental::MLIR_BRIDGE_ROLLOUT_UNSPECIFIED;
  }

  // TF1 graphs that do override Session's ConfigProto and set
  // ConfigProto's enable_mlir_bridge or mlir_bridge_rollout fields will not
  // update tf_mlir_enable_mlir_bridge so check their values.

  // ConfigProto's enable_mlir_bridge defaults to false so only respect it
  // when it is true.
  if (config_proto.value().experimental().enable_mlir_bridge()) {
    return ConfigProto::Experimental::MLIR_BRIDGE_ROLLOUT_ENABLED;
  }
  return config_proto.value().experimental().mlir_bridge_rollout();
}

void AppendMarkForCompilationPassFlags(std::vector<Flag>* flag_list) {
  absl::call_once(flags_init, &AllocateAndParseFlags);
  AppendMarkForCompilationPassFlagsInternal(flag_list);
}

static std::atomic<bool> xla_compilation_disabled(false);

void DisableXlaCompilation() { xla_compilation_disabled = true; }

void EnableXlaCompilation() { xla_compilation_disabled = false; }

bool FailOnXlaCompilation() { return xla_compilation_disabled; }

}  // namespace tensorflow

/* Copyright 2022 The TensorFlow Authors. All Rights Reserved.

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

#include <memory>
#include <string>

#include "absl/types/optional.h"
#include "pybind11/pybind11.h"  // from @pybind11
#include "pybind11/pytypes.h"  // from @pybind11
#include "pybind11/stl.h"  // from @pybind11
#include "tensorflow/core/config/flag_defs.h"
#include "tensorflow/core/config/flags.h"

namespace py = pybind11;
namespace flags = tensorflow::flags;

using tensorflow::config::Flag;
using tensorflow::flags::Flags;

// Macro to expose the declared flag to python.
#define TF_PY_DECLARE_FLAG(flag_name) \
  flags.def_readwrite(#flag_name, &Flags::flag_name);

PYBIND11_MODULE(flags_pybind, m) {
  py::class_<Flag>(m, "Flag")
      .def("value", &Flag::value)
      .def("reset", &Flag::reset);

  py::class_<Flags, std::unique_ptr<Flags, py::nodelete>> flags(m, "Flags");
  flags.def(py::init(
      []() { return std::unique_ptr<Flags, py::nodelete>(&flags::Global()); }));
  // LINT.IfChange
  TF_PY_DECLARE_FLAG(test_only_experiment_1);
  TF_PY_DECLARE_FLAG(test_only_experiment_2);
  TF_PY_DECLARE_FLAG(enable_nested_function_shape_inference);
  TF_PY_DECLARE_FLAG(enable_quantized_dtypes_training);
  TF_PY_DECLARE_FLAG(graph_building_optimization);
  TF_PY_DECLARE_FLAG(op_building_optimization);
  TF_PY_DECLARE_FLAG(saved_model_fingerprinting);
  TF_PY_DECLARE_FLAG(tf_shape_default_int64);
  TF_PY_DECLARE_FLAG(more_stack_traces);
  TF_PY_DECLARE_FLAG(publish_function_graphs);
  TF_PY_DECLARE_FLAG(enable_aggressive_constant_replication);
  TF_PY_DECLARE_FLAG(enable_colocation_key_propagation_in_while_op_lowering);
  // LINT.ThenChange(//tensorflow/core/config/flag_defs.h)
};
