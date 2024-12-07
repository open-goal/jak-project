#include "animation_processing.h"

#include "common/log/log.h"
#include "common/util/gltf_util.h"

namespace anim {

namespace {
int find_max_joint(const tinygltf::Animation& anim, const std::map<int, int>& node_to_joint) {
  int max_joint = 0;
  for (const auto& channel : anim.channels) {
    if (node_to_joint.find(channel.target_node) != node_to_joint.end()) {
      max_joint = std::max(node_to_joint.at(channel.target_node), max_joint);
    } else {
      lg::error("animated node not in our skeleton!!");
    }
  }
  return max_joint;
}

template <typename T>
std::vector<T> compute_keyframes(const std::vector<float>& times,
                                 const std::vector<T>& values,
                                 float framerate,
                                 bool quaternion_interp) {
  std::vector<T> ret;
  ASSERT(!times.empty());
  ASSERT(times.size() == values.size());
  size_t i = 0;
  float t = 0;
  while (t < times.back()) {
    // advance input keyframe
    while ((i + 1 < times.size())  // can advance
           && times.at(i + 1) < t  // next keyframe is before sample point
    ) {
      i++;
    }

    const float fraction = (t - times.at(i)) / (times.at(i + 1) - times.at(i));
    if (quaternion_interp) {
      float multiplier = 1;
      if (values.at(i).dot(values.at(i + 1)) < 0) {
        multiplier = -1;
      }
      ret.push_back(values.at(i) * (1.f - fraction) + values.at(i + 1) * fraction * multiplier);
    } else {
      ret.push_back(values.at(i) * (1.f - fraction) + values.at(i + 1) * fraction);
    }
    t += 1.f / framerate;
  }
  return ret;
}

template <int n>
std::vector<math::Vector<float, n>> extract_keyframed_gltf_vecn(
    const tinygltf::Model& model,
    const tinygltf::AnimationSampler& sampler,
    float framerate,
    bool quaternion_interp) {
  std::vector<float> times = gltf_util::extract_floats(model, sampler.input);
  std::vector<math::Vector<float, n>> values =
      gltf_util::extract_vec<float, n>(model, sampler.output, TINYGLTF_COMPONENT_TYPE_FLOAT);
  ASSERT(times.size() == values.size());
  return compute_keyframes(times, values, framerate, quaternion_interp);
}
}  // namespace

UncompressedJointAnim extract_anim_from_gltf(const tinygltf::Model& model,
                                             const tinygltf::Animation& anim,
                                             const std::map<int, int>& node_to_joint,
                                             float framerate) {
  UncompressedJointAnim out;
  out.name = anim.name;
  lg::info("Processing animation {}", anim.name);
  const int max_joint = find_max_joint(anim, node_to_joint);
  lg::info("Max joint is {}", max_joint);
  out.joints.resize(max_joint + 1);

  for (const auto& channel : anim.channels) {
    const int channel_node_idx = channel.target_node;
    // const auto& channel_node = model.nodes.at(channel_node_idx);
    const int channel_joint = node_to_joint.at(channel_node_idx);
    // lg::info("channel for {} {} / {}", channel_joint, channel_node.name, channel.target_path);
    const auto& sampler = anim.samplers.at(channel.sampler);
    if (channel.target_path == "translation") {
      out.joints.at(channel_joint).trans_frames =
          extract_keyframed_gltf_vecn<3>(model, sampler, framerate, false);
    } else if (channel.target_path == "rotation") {
      out.joints.at(channel_joint).quat_frames =
          extract_keyframed_gltf_vecn<4>(model, sampler, framerate, true);
    } else if (channel.target_path == "scale") {
      out.joints.at(channel_joint).scale_frames =
          extract_keyframed_gltf_vecn<3>(model, sampler, framerate, false);
    } else {
      lg::die("unknown target_path {}", channel.target_path);
    }
  }

  size_t max_frames = 0;
  for (auto& joint : out.joints) {
    max_frames = std::max(max_frames, joint.quat_frames.size());
    max_frames = std::max(max_frames, joint.trans_frames.size());
    max_frames = std::max(max_frames, joint.scale_frames.size());
  }
  lg::info("max frames is {}", max_frames);

  // make up data for missing joints (like align, for example)
  for (auto& joint : out.joints) {
    if (joint.quat_frames.size() < max_frames) {
      lg::warn("joint with {} / {} quat frames!", joint.quat_frames.size(), max_frames);
      if (joint.quat_frames.empty()) {
        joint.quat_frames.emplace_back(0, 0, 0, 1);
      }
      while (joint.quat_frames.size() < max_frames) {
        joint.quat_frames.push_back(joint.quat_frames.back());
      }
    }

    if (joint.trans_frames.size() < max_frames) {
      lg::warn("joint with {} / {} trans frames!", joint.trans_frames.size(), max_frames);
      if (joint.trans_frames.empty()) {
        joint.trans_frames.emplace_back(0, 0, 0);
      }
      while (joint.trans_frames.size() < max_frames) {
        joint.trans_frames.push_back(joint.trans_frames.back());
      }
    }

    if (joint.scale_frames.size() < max_frames) {
      lg::warn("joint with {} / {} scale frames!", joint.scale_frames.size(), max_frames);
      if (joint.scale_frames.empty()) {
        joint.scale_frames.emplace_back(1, 1, 1);
      }
      while (joint.scale_frames.size() < max_frames) {
        joint.scale_frames.push_back(joint.scale_frames.back());
      }
    }
  }

  out.framerate = framerate;
  out.frames = max_frames;
  return out;
}

namespace {
template <int n>
bool is_constant(const std::vector<math::Vector<float, n>>& in) {
  if (in.empty()) {
    return true;
  }
  auto first = in.at(0);
  for (auto& x : in) {
    if (x != first) {
      return false;
    }
  }
  return true;
}

bool can_use_small_trans(const std::vector<math::Vector3f>& trans_frames) {
  constexpr float kMaxTrans = 32767.f * (4.f / 4096.f);
  for (auto& trans : trans_frames) {
    for (int i = 0; i < 3; i++) {
      if (trans[i] > kMaxTrans || trans[i] < -kMaxTrans) {
        return false;
      }
    }
  }
  return true;
}

bool is_matrix_constant(const UncompressedSingleJointAnim& anim) {
  return is_constant(anim.quat_frames) && is_constant(anim.scale_frames) &&
         is_constant(anim.trans_frames);
}

void compress_frame_to_matrix(CompressedFrame* frame,
                              const math::Vector3f& trans,
                              const math::Vector4f& quat,
                              const math::Vector3f& scale) {
  auto mat = gltf_util::matrix_from_trs(trans * 4096, quat, scale);
  constexpr int n = 4 * 4 * sizeof(float) / sizeof(u64);
  u64 data[n];
  memcpy(data, mat.data(), 4 * 4 * sizeof(float));
  frame->data64.insert(frame->data64.end(), data, data + n);
}

void compress_trans(CompressedFrame* frame, const math::Vector3f& trans, bool big) {
  if (big) {
    // 64, 64, 32
    u64 data[1];
    memcpy(data, trans.data(), 2 * sizeof(float));
    frame->data64.push_back(data[0]);
    u32 data_32[1];
    memcpy(data_32, &trans.z(), sizeof(float));
    frame->data32.push_back(data_32[0]);
  } else {
    constexpr float kTransScale = 4.f / 4096.f;
    s16 data1[3];
    for (int i = 0; i < 3; i++) {
      data1[i] = s16(trans[i] / kTransScale);
    }
    u32 data2[1];
    memcpy(data2, data1, 4);
    frame->data32.push_back(data2[0]);
    frame->data16.push_back(data1[2]);
  }
}

void compress_quat(CompressedFrame* frame, const math::Vector4f& quat) {
  constexpr float kQuatScale = 0.000030517578125;
  s16 data1[4];
  for (int i = 0; i < 4; i++) {
    data1[i] = s16(quat[i] / kQuatScale);
  }
  u64 data2[1];
  memcpy(data2, data1, 8);
  frame->data64.push_back(data2[0]);
}

void compress_scale(CompressedFrame* frame, const math::Vector3f& scale) {
  constexpr float kScaleScale = 0.000244140625;
  s16 data1[3];
  for (int i = 0; i < 3; i++) {
    data1[i] = s16(scale[i] / kScaleScale);
  }
  u32 data2[1];
  memcpy(data2, data1, 4);
  frame->data32.push_back(data2[0]);
  frame->data16.push_back(data1[2]);
}
}  // namespace

CompressedAnim compress_animation(const UncompressedJointAnim& in) {
  ASSERT(in.joints.size() >= 2);  // need two matrix joints.
  CompressedAnim out;
  out.name = in.name;
  out.framerate = in.framerate;
  out.frames.resize(in.frames);
  for (int matrix = 0; matrix < 2; matrix++) {
    const auto& joint_data = in.joints.at(matrix);
    if (is_matrix_constant(joint_data)) {
      out.matrix_animated[matrix] = false;
      compress_frame_to_matrix(&out.fixed, joint_data.trans_frames[0], joint_data.quat_frames[0],
                               joint_data.scale_frames[0]);
    } else {
      out.matrix_animated[matrix] = true;
      for (int i = 0; i < in.frames; i++) {
        compress_frame_to_matrix(&out.frames[i], joint_data.trans_frames[i],
                                 joint_data.quat_frames[i], joint_data.scale_frames[i]);
      }
    }
  }

  for (size_t joint = 2; joint < in.joints.size(); joint++) {
    const auto& joint_data = in.joints.at(joint);
    auto& metadata = out.joint_metadata.emplace_back();

    metadata.animated_trans = !is_constant(joint_data.trans_frames);
    metadata.animated_quat = !is_constant(joint_data.quat_frames);
    metadata.animated_scale = !is_constant(joint_data.scale_frames);
    metadata.big_trans_mode = !can_use_small_trans(joint_data.trans_frames);

    if (metadata.animated_trans) {
      for (int i = 0; i < in.frames; i++) {
        compress_trans(&out.frames[i], joint_data.trans_frames[i], metadata.big_trans_mode);
      }
    } else {
      compress_trans(&out.fixed, joint_data.trans_frames[0], metadata.big_trans_mode);
    }

    if (metadata.animated_quat) {
      for (int i = 0; i < in.frames; i++) {
        compress_quat(&out.frames[i], joint_data.quat_frames[i]);
      }
    } else {
      compress_quat(&out.fixed, joint_data.quat_frames[0]);
    }

    if (metadata.animated_scale) {
      for (int i = 0; i < in.frames; i++) {
        compress_scale(&out.frames[i], joint_data.scale_frames[i]);
      }
    } else {
      compress_scale(&out.fixed, joint_data.scale_frames[0]);
    }
  }

  lg::info("animation {} size {:.2f} kB", in.name,
           (out.fixed.size_bytes() + out.frames.size() * out.frames.at(0).size_bytes()) / 1024.f);
  return out;
}
}  // namespace anim