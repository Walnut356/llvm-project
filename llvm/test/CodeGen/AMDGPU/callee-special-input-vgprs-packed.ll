; RUN: opt -passes=amdgpu-attributor -mcpu=kaveri < %s | llc -mcpu=gfx90a -enable-ipra=0 | FileCheck -enable-var-scope -check-prefixes=GCN,GFX7,UNPACKED-TID %s
; RUN: opt -passes=amdgpu-attributor -mcpu=gfx90a -mattr=-xnack < %s | llc -mcpu=gfx90a -mattr=-xnack -enable-ipra=0 | FileCheck -enable-var-scope -check-prefixes=GCN,GFX90A,PACKED-TID %s

target triple = "amdgcn-amd-amdhsa"

; GCN-LABEL: {{^}}use_workitem_id_x:
; GCN: s_waitcnt
; GCN: v_and_b32_e32 [[ID:v[0-9]+]], 0x3ff, v31
; GCN-NEXT: {{flat|global}}_store_dword v{{\[[0-9]:[0-9]+\]}}, [[ID]]
; GCN-NEXT: s_waitcnt
; GCN-NEXT: s_setpc_b64
define void @use_workitem_id_x() #1 {
  %val = call i32 @llvm.amdgcn.workitem.id.x()
  store volatile i32 %val, ptr addrspace(1) poison
  ret void
}

; GCN-LABEL: {{^}}use_workitem_id_y:
; GCN: s_waitcnt
; GCN: v_bfe_u32 [[ID:v[0-9]+]], v31, 10, 10
; GCN-NEXT: {{flat|global}}_store_dword v{{\[[0-9]:[0-9]+\]}}, [[ID]]
; GCN-NEXT: s_waitcnt
; GCN-NEXT: s_setpc_b64
define void @use_workitem_id_y() #1 {
  %val = call i32 @llvm.amdgcn.workitem.id.y()
  store volatile i32 %val, ptr addrspace(1) poison
  ret void
}

; GCN-LABEL: {{^}}use_workitem_id_z:
; GCN: s_waitcnt
; GCN: v_bfe_u32 [[ID:v[0-9]+]], v31, 20, 10
; GCN-NEXT: {{flat|global}}_store_dword v{{\[[0-9]:[0-9]+\]}}, [[ID]]
; GCN-NEXT: s_waitcnt
; GCN-NEXT: s_setpc_b64
define void @use_workitem_id_z() #1 {
  %val = call i32 @llvm.amdgcn.workitem.id.z()
  store volatile i32 %val, ptr addrspace(1) poison
  ret void
}

; GCN-LABEL: {{^}}use_workitem_id_xy:
; GCN: s_waitcnt
; GCN-DAG: v_and_b32_e32 [[IDX:v[0-9]+]], 0x3ff, v31
; GCN-DAG: v_bfe_u32 [[IDY:v[0-9]+]], v31, 10, 10
; GCN-DAG: {{flat|global}}_store_dword v{{\[[0-9]:[0-9]+\]}}, [[IDX]]
; GCN-DAG: {{flat|global}}_store_dword v{{\[[0-9]:[0-9]+\]}}, [[IDY]]
; GCN-NEXT: s_waitcnt
; GCN-NEXT: s_setpc_b64
define void @use_workitem_id_xy() #1 {
  %val0 = call i32 @llvm.amdgcn.workitem.id.x()
  %val1 = call i32 @llvm.amdgcn.workitem.id.y()
  store volatile i32 %val0, ptr addrspace(1) poison
  store volatile i32 %val1, ptr addrspace(1) poison
  ret void
}

; GCN-LABEL: {{^}}use_workitem_id_xyz:
; GCN: s_waitcnt
; GCN-DAG: v_and_b32_e32 [[IDX:v[0-9]+]], 0x3ff, v31
; GCN-DAG: v_bfe_u32 [[IDY:v[0-9]+]], v31, 10, 10
; GCN-DAG: v_bfe_u32 [[IDZ:v[0-9]+]], v31, 20, 10
; GCN-DAG: {{flat|global}}_store_dword v{{\[[0-9]:[0-9]+\]}}, [[IDX]]
; GCN-DAG: {{flat|global}}_store_dword v{{\[[0-9]:[0-9]+\]}}, [[IDY]]
; GCN-DAG: {{flat|global}}_store_dword v{{\[[0-9]:[0-9]+\]}}, [[IDZ]]
; GCN-NEXT: s_waitcnt
; GCN-NEXT: s_setpc_b64
define void @use_workitem_id_xyz() #1 {
  %val0 = call i32 @llvm.amdgcn.workitem.id.x()
  %val1 = call i32 @llvm.amdgcn.workitem.id.y()
  %val2 = call i32 @llvm.amdgcn.workitem.id.z()
  store volatile i32 %val0, ptr addrspace(1) poison
  store volatile i32 %val1, ptr addrspace(1) poison
  store volatile i32 %val2, ptr addrspace(1) poison
  ret void
}

; GCN-LABEL: {{^}}use_workitem_id_xz:
; GCN: s_waitcnt
; GCN-DAG: v_and_b32_e32 [[IDX:v[0-9]+]], 0x3ff, v31
; GCN-DAG: v_bfe_u32 [[IDZ:v[0-9]+]], v31, 20, 10
; GCN-DAG: {{flat|global}}_store_dword v{{\[[0-9]:[0-9]+\]}}, [[IDX]]
; GCN-DAG: {{flat|global}}_store_dword v{{\[[0-9]:[0-9]+\]}}, [[IDZ]]
; GCN-NEXT: s_waitcnt
; GCN-NEXT: s_setpc_b64
define void @use_workitem_id_xz() #1 {
  %val0 = call i32 @llvm.amdgcn.workitem.id.x()
  %val1 = call i32 @llvm.amdgcn.workitem.id.z()
  store volatile i32 %val0, ptr addrspace(1) poison
  store volatile i32 %val1, ptr addrspace(1) poison
  ret void
}

; GCN-LABEL: {{^}}use_workitem_id_yz:
; GCN: s_waitcnt
; GCN-DAG: v_bfe_u32 [[IDY:v[0-9]+]], v31, 10, 10
; GCN-DAG: v_bfe_u32 [[IDZ:v[0-9]+]], v31, 20, 10
; GCN-DAG: {{flat|global}}_store_dword v{{\[[0-9]:[0-9]+\]}}, [[IDY]]
; GCN-DAG: {{flat|global}}_store_dword v{{\[[0-9]:[0-9]+\]}}, [[IDZ]]
; GCN-NEXT: s_waitcnt
; GCN-NEXT: s_setpc_b64
define void @use_workitem_id_yz() #1 {
  %val0 = call i32 @llvm.amdgcn.workitem.id.y()
  %val1 = call i32 @llvm.amdgcn.workitem.id.z()
  store volatile i32 %val0, ptr addrspace(1) poison
  store volatile i32 %val1, ptr addrspace(1) poison
  ret void
}

; GCN-LABEL: {{^}}kern_indirect_use_workitem_id_x:
; GCN: v_mov_b32_e32 v31, v0
; GCN: s_swappc_b64
; GCN-NOT: v31

; GCN: .amdhsa_system_vgpr_workitem_id 0
define amdgpu_kernel void @kern_indirect_use_workitem_id_x() #1 {
  call void @use_workitem_id_x()
  ret void
}

; GCN-LABEL: {{^}}kern_indirect_use_workitem_id_y:

; GCN-NOT: v0
; GCN-NOT: v1
; GCN-NOT: v31
; PACKED-TID: v_mov_b32_e32 v31, v0
; UNPACKED-TID: v_lshlrev_b32_e32 v31, 10, v1
; GCN-NOT: v0
; GCN-NOT: v1
; GCN: s_swappc_b64

; GCN: .amdhsa_system_vgpr_workitem_id 1
define amdgpu_kernel void @kern_indirect_use_workitem_id_y() #1 {
  call void @use_workitem_id_y()
  ret void
}

; GCN-LABEL: {{^}}kern_indirect_use_workitem_id_z:

; GCN-NOT: v0
; GCN-NOT: v2
; GCN-NOT: v31
; PACKED-TID: v_mov_b32_e32 v31, v0
; UNPACKED-TID: v_lshlrev_b32_e32 v31, 20, v2
; GCN-NOT: v0
; GCN-NOT: v1
; GCN: s_swappc_b64

; GCN: .amdhsa_system_vgpr_workitem_id 2
define amdgpu_kernel void @kern_indirect_use_workitem_id_z() #1 {
  call void @use_workitem_id_z()
  ret void
}

; GCN-LABEL: {{^}}kern_indirect_use_workitem_id_xy:
; GCN-NOT: v0
; GCN-NOT: v1
; PACKED-TID: v_mov_b32_e32 v31, v0
; UNPACKED-TID: v_lshlrev_b32_e32 [[IDY:v[0-9]+]], 10, v1
; UNPACKED-TID: v_or_b32_e32 v31, v0, [[IDY]]
; GCN-NOT: v0
; GCN-NOT: v1
; GCN: s_swappc_b64
define amdgpu_kernel void @kern_indirect_use_workitem_id_xy() #1 {
  call void @use_workitem_id_xy()
  ret void
}

; GCN-LABEL: {{^}}kern_indirect_use_workitem_id_xz:
; GCN-NOT: v0
; GCN-NOT: v2

; PACKED-TID: v_mov_b32_e32 v31, v0
; UNPACKED-TID: v_lshlrev_b32_e32 [[IDZ:v[0-9]+]], 20, v2
; UNPACKED-TID: v_or_b32_e32 v31, v0, [[IDZ]]
; GCN-NOT: v0
; GCN-NOT: v2
; GCN: s_swappc_b64
define amdgpu_kernel void @kern_indirect_use_workitem_id_xz() #1 {
  call void @use_workitem_id_xz()
  ret void
}

; GCN-LABEL: {{^}}kern_indirect_use_workitem_id_yz:
; GCN-NOT: v1
; GCN-NOT: v2
; PACKED-TID: v_mov_b32_e32 v31, v0
; UNPACKED-TID-DAG: v_lshlrev_b32_e32 [[IDY:v[0-9]+]], 10, v1
; UNPACKED-TID-DAG: v_lshlrev_b32_e32 [[IDZ:v[0-9]+]], 20, v2
; UNPACKED-TID: v_or_b32_e32 v31, [[IDY]], [[IDZ]]
; GCN-NOT: v1
; GCN-NOT: v2
; GCN: s_swappc_b64
define amdgpu_kernel void @kern_indirect_use_workitem_id_yz() #1 {
  call void @use_workitem_id_yz()
  ret void
}

; GCN-LABEL: {{^}}kern_indirect_use_workitem_id_xyz:
; GCN-NOT: v0
; GCN-NOT: v1
; GCN-NOT: v2

; PACKED-TID: v_mov_b32_e32 v31, v0

; UNPACKED-TID-DAG: v_lshlrev_b32_e32 [[IDY:v[0-9]+]], 10, v1
; UNPACKED-TID-DAG: v_lshlrev_b32_e32 [[IDZ:v[0-9]+]], 20, v2
; UNPACKED-TID-DAG: v_or_b32_e32 v0, v0, [[IDY]]
; UNPACKED-TID-DAG: v_or_b32_e32 v31, v0, [[IDZ]]
; GCN-NOT: v0
; GCN-NOT: v1
; GCN-NOT: v2
; GCN: s_swappc_b64
define amdgpu_kernel void @kern_indirect_use_workitem_id_xyz() #1 {
  call void @use_workitem_id_xyz()
  ret void
}

; GCN-LABEL: {{^}}func_indirect_use_workitem_id_x:
; GCN-NOT: v0
; GCN: s_swappc_b64
; GCN-NOT: v0
define void @func_indirect_use_workitem_id_x() #1 {
  call void @use_workitem_id_x()
  ret void
}

; GCN-LABEL: {{^}}func_indirect_use_workitem_id_y:
; GCN-NOT: v0
; GCN: s_swappc_b64
; GCN-NOT: v0
define void @func_indirect_use_workitem_id_y() #1 {
  call void @use_workitem_id_y()
  ret void
}

; GCN-LABEL: {{^}}func_indirect_use_workitem_id_z:
; GCN-NOT: v0
; GCN: s_swappc_b64
; GCN-NOT: v0
define void @func_indirect_use_workitem_id_z() #1 {
  call void @use_workitem_id_z()
  ret void
}

; GCN-LABEL: {{^}}other_arg_use_workitem_id_x:
; GCN: s_waitcnt
; GCN-DAG: v_and_b32_e32 [[ID:v[0-9]+]], 0x3ff, v31
; GCN-DAG: {{flat|global}}_store_dword v{{\[[0-9]+:[0-9]+\]}}, v0
; GCN-DAG: {{flat|global}}_store_dword v{{\[[0-9]+:[0-9]+\]}}, [[ID]]
define void @other_arg_use_workitem_id_x(i32 %arg0) #1 {
  %val = call i32 @llvm.amdgcn.workitem.id.x()
  store volatile i32 %arg0, ptr addrspace(1) poison
  store volatile i32 %val, ptr addrspace(1) poison
  ret void
}

; GCN-LABEL: {{^}}other_arg_use_workitem_id_y:
; GCN: s_waitcnt
; GCN-DAG: v_bfe_u32 [[ID:v[0-9]+]], v31, 10, 10
; GCN-DAG: {{flat|global}}_store_dword v{{\[[0-9]+:[0-9]+\]}}, v0
; GCN-DAG: {{flat|global}}_store_dword v{{\[[0-9]+:[0-9]+\]}}, [[ID]]
define void @other_arg_use_workitem_id_y(i32 %arg0) #1 {
  %val = call i32 @llvm.amdgcn.workitem.id.y()
  store volatile i32 %arg0, ptr addrspace(1) poison
  store volatile i32 %val, ptr addrspace(1) poison
  ret void
}

; GCN-LABEL: {{^}}other_arg_use_workitem_id_z:
; GCN: s_waitcnt
; GCN-DAG: v_bfe_u32 [[ID:v[0-9]+]], v31, 20, 10
; GCN-DAG: {{flat|global}}_store_dword v{{\[[0-9]+:[0-9]+\]}}, v0
; GCN-DAG: {{flat|global}}_store_dword v{{\[[0-9]+:[0-9]+\]}}, [[ID]]
define void @other_arg_use_workitem_id_z(i32 %arg0) #1 {
  %val = call i32 @llvm.amdgcn.workitem.id.z()
  store volatile i32 %arg0, ptr addrspace(1) poison
  store volatile i32 %val, ptr addrspace(1) poison
  ret void
}


; GCN-LABEL: {{^}}kern_indirect_other_arg_use_workitem_id_x:

; GCN: v_mov_b32_e32 v31, v0
; GCN: v_mov_b32_e32 v0, 0x22b
; GCN: s_swappc_b64

; GCN: .amdhsa_system_vgpr_workitem_id 0
define amdgpu_kernel void @kern_indirect_other_arg_use_workitem_id_x() #1 {
  call void @other_arg_use_workitem_id_x(i32 555)
  ret void
}


; GCN-LABEL: {{^}}kern_indirect_other_arg_use_workitem_id_y:

; UNPACKED-TID: v_lshlrev_b32_e32 v31, 10, v1
; PACKED-TID:   v_mov_b32_e32 v31, v0
; GCN-NOT: v1
; GCN: v_mov_b32_e32 v0, 0x22b
; GCN-NOT: v1
; GCN: s_swappc_b64
; GCN-NOT: v0

; GCN: .amdhsa_system_vgpr_workitem_id 1
define amdgpu_kernel void @kern_indirect_other_arg_use_workitem_id_y() #1 {
  call void @other_arg_use_workitem_id_y(i32 555)
  ret void
}

; GCN-LABEL: {{^}}kern_indirect_other_arg_use_workitem_id_z:

; GCN-DAG: v_mov_b32_e32 v0, 0x22b
; UNPACKED-TID-DAG: v_lshlrev_b32_e32 v31, 20, v2
; PACKED-TID-DAG: v_mov_b32_e32 v31, v0
; GCN: s_swappc_b64
; GCN-NOT: v0

; GCN: .amdhsa_system_vgpr_workitem_id 2
define amdgpu_kernel void @kern_indirect_other_arg_use_workitem_id_z() #1 {
  call void @other_arg_use_workitem_id_z(i32 555)
  ret void
}

; GCN-LABEL: {{^}}too_many_args_use_workitem_id_x:
; GCN-DAG: v_and_b32_e32 v31, 0x3ff, v31
; GCN-DAG: buffer_load_dword [[LOAD_ARG31:v[0-9]+]], off, s[0:3], s32{{$}}
; GCN: {{flat|global}}_store_dword v{{\[[0-9]+:[0-9]+]}}, [[LOAD_ARG31]]
; GCN-NEXT: s_waitcnt
; GCN-NEXT: s_setpc_b64
define void @too_many_args_use_workitem_id_x(
  i32 %arg0, i32 %arg1, i32 %arg2, i32 %arg3, i32 %arg4, i32 %arg5, i32 %arg6, i32 %arg7,
  i32 %arg8, i32 %arg9, i32 %arg10, i32 %arg11, i32 %arg12, i32 %arg13, i32 %arg14, i32 %arg15,
  i32 %arg16, i32 %arg17, i32 %arg18, i32 %arg19, i32 %arg20, i32 %arg21, i32 %arg22, i32 %arg23,
  i32 %arg24, i32 %arg25, i32 %arg26, i32 %arg27, i32 %arg28, i32 %arg29, i32 %arg30, i32 %arg31) #1 {
  %val = call i32 @llvm.amdgcn.workitem.id.x()
  store volatile i32 %val, ptr addrspace(1) poison

  store volatile i32 %arg0, ptr addrspace(1) poison
  store volatile i32 %arg1, ptr addrspace(1) poison
  store volatile i32 %arg2, ptr addrspace(1) poison
  store volatile i32 %arg3, ptr addrspace(1) poison
  store volatile i32 %arg4, ptr addrspace(1) poison
  store volatile i32 %arg5, ptr addrspace(1) poison
  store volatile i32 %arg6, ptr addrspace(1) poison
  store volatile i32 %arg7, ptr addrspace(1) poison

  store volatile i32 %arg8, ptr addrspace(1) poison
  store volatile i32 %arg9, ptr addrspace(1) poison
  store volatile i32 %arg10, ptr addrspace(1) poison
  store volatile i32 %arg11, ptr addrspace(1) poison
  store volatile i32 %arg12, ptr addrspace(1) poison
  store volatile i32 %arg13, ptr addrspace(1) poison
  store volatile i32 %arg14, ptr addrspace(1) poison
  store volatile i32 %arg15, ptr addrspace(1) poison

  store volatile i32 %arg16, ptr addrspace(1) poison
  store volatile i32 %arg17, ptr addrspace(1) poison
  store volatile i32 %arg18, ptr addrspace(1) poison
  store volatile i32 %arg19, ptr addrspace(1) poison
  store volatile i32 %arg20, ptr addrspace(1) poison
  store volatile i32 %arg21, ptr addrspace(1) poison
  store volatile i32 %arg22, ptr addrspace(1) poison
  store volatile i32 %arg23, ptr addrspace(1) poison

  store volatile i32 %arg24, ptr addrspace(1) poison
  store volatile i32 %arg25, ptr addrspace(1) poison
  store volatile i32 %arg26, ptr addrspace(1) poison
  store volatile i32 %arg27, ptr addrspace(1) poison
  store volatile i32 %arg28, ptr addrspace(1) poison
  store volatile i32 %arg29, ptr addrspace(1) poison
  store volatile i32 %arg30, ptr addrspace(1) poison
  store volatile i32 %arg31, ptr addrspace(1) poison

  ret void
}

; GCN-LABEL: {{^}}kern_call_too_many_args_use_workitem_id_x:

; GCN: s_mov_b32 s32, 0
; GCN: buffer_store_dword v1, off, s[0:3], s32{{$}}
; GCN: v_mov_b32_e32 v31, v0
; GCN: s_swappc_b64

; GCN: .amdhsa_system_vgpr_workitem_id 0
define amdgpu_kernel void @kern_call_too_many_args_use_workitem_id_x() #1 {
  call void @too_many_args_use_workitem_id_x(
    i32 10, i32 20, i32 30, i32 40,
    i32 50, i32 60, i32 70, i32 80,
    i32 90, i32 100, i32 110, i32 120,
    i32 130, i32 140, i32 150, i32 160,
    i32 170, i32 180, i32 190, i32 200,
    i32 210, i32 220, i32 230, i32 240,
    i32 250, i32 260, i32 270, i32 280,
    i32 290, i32 300, i32 310, i32 320)
  ret void
}

; GCN-LABEL: {{^}}func_call_too_many_args_use_workitem_id_x:
; GCN-NOT: v31
; GCN: s_mov_b32 s33, s32
; GCN: buffer_store_dword v0, off, s[0:3], s32{{$}}
; GCN-NOT: v31
; GCN: s_swappc_b64
; GCN-NOT: v31
define void @func_call_too_many_args_use_workitem_id_x(i32 %arg0) #1 {
  store volatile i32 %arg0, ptr addrspace(1) poison
  call void @too_many_args_use_workitem_id_x(
    i32 10, i32 20, i32 30, i32 40,
    i32 50, i32 60, i32 70, i32 80,
    i32 90, i32 100, i32 110, i32 120,
    i32 130, i32 140, i32 150, i32 160,
    i32 170, i32 180, i32 190, i32 200,
    i32 210, i32 220, i32 230, i32 240,
    i32 250, i32 260, i32 270, i32 280,
    i32 290, i32 300, i32 310, i32 320)
  ret void
}

; Requires loading and storing to stack slot.
; GCN-LABEL: {{^}}too_many_args_call_too_many_args_use_workitem_id_x:
; GCN-DAG: s_addk_i32 s32, 0x400{{$}}
; GCN-DAG: buffer_store_dword v40, off, s[0:3], s33 offset:4 ; 4-byte Folded Spill
; GCN-DAG: buffer_load_dword [[TMP_REG:v[0-9]+]], off, s[0:3], s33{{$}}

; GCN: buffer_store_dword [[TMP_REG]], off, s[0:3], s32{{$}}

; GCN: s_swappc_b64

; GCN: s_mov_b32 s32, s33
; GCN: buffer_load_dword v40, off, s[0:3], s33 offset:4 ; 4-byte Folded Reload
; GCN: s_setpc_b64
define void @too_many_args_call_too_many_args_use_workitem_id_x(
  i32 %arg0, i32 %arg1, i32 %arg2, i32 %arg3, i32 %arg4, i32 %arg5, i32 %arg6, i32 %arg7,
  i32 %arg8, i32 %arg9, i32 %arg10, i32 %arg11, i32 %arg12, i32 %arg13, i32 %arg14, i32 %arg15,
  i32 %arg16, i32 %arg17, i32 %arg18, i32 %arg19, i32 %arg20, i32 %arg21, i32 %arg22, i32 %arg23,
  i32 %arg24, i32 %arg25, i32 %arg26, i32 %arg27, i32 %arg28, i32 %arg29, i32 %arg30, i32 %arg31) #1 {
  call void @too_many_args_use_workitem_id_x(
    i32 %arg0, i32 %arg1, i32 %arg2, i32 %arg3, i32 %arg4, i32 %arg5, i32 %arg6, i32 %arg7,
    i32 %arg8, i32 %arg9, i32 %arg10, i32 %arg11, i32 %arg12, i32 %arg13, i32 %arg14, i32 %arg15,
    i32 %arg16, i32 %arg17, i32 %arg18, i32 %arg19, i32 %arg20, i32 %arg21, i32 %arg22, i32 %arg23,
    i32 %arg24, i32 %arg25, i32 %arg26, i32 %arg27, i32 %arg28, i32 %arg29, i32 %arg30, i32 %arg31)
  ret void
}

; stack layout:
; frame[0] = stack passed arg23
; frame[1] = byval arg32

; GCN-LABEL: {{^}}too_many_args_use_workitem_id_x_byval:
; GCN-DAG: v_and_b32_e32 v31, 0x3ff, v31
; GCN-DAG: {{flat|global}}_store_dword v{{\[[0-9]+:[0-9]+\]}}, v31
; GCN-DAG: buffer_load_dword [[LOAD_ARG31:v[0-9]+]], off, s[0:3], s32{{$}}
; GCN: {{flat|global}}_store_dword v{{\[[0-9]+:[0-9]+\]}}, [[LOAD_ARG31]]
; GCN: buffer_load_dword v{{[0-9]+}}, off, s[0:3], s32 offset:4 glc
; GCN-NEXT: s_waitcnt
; GCN-NEXT: s_setpc_b64
define void @too_many_args_use_workitem_id_x_byval(
  i32 %arg0, i32 %arg1, i32 %arg2, i32 %arg3, i32 %arg4, i32 %arg5, i32 %arg6, i32 %arg7,
  i32 %arg8, i32 %arg9, i32 %arg10, i32 %arg11, i32 %arg12, i32 %arg13, i32 %arg14, i32 %arg15,
  i32 %arg16, i32 %arg17, i32 %arg18, i32 %arg19, i32 %arg20, i32 %arg21, i32 %arg22, i32 %arg23,
  i32 %arg24, i32 %arg25, i32 %arg26, i32 %arg27, i32 %arg28, i32 %arg29, i32 %arg30, i32 %arg31, ptr addrspace(5) byval(i32) %arg32) #1 {
  %val = call i32 @llvm.amdgcn.workitem.id.x()
  store volatile i32 %val, ptr addrspace(1) poison

  store volatile i32 %arg0, ptr addrspace(1) poison
  store volatile i32 %arg1, ptr addrspace(1) poison
  store volatile i32 %arg2, ptr addrspace(1) poison
  store volatile i32 %arg3, ptr addrspace(1) poison
  store volatile i32 %arg4, ptr addrspace(1) poison
  store volatile i32 %arg5, ptr addrspace(1) poison
  store volatile i32 %arg6, ptr addrspace(1) poison
  store volatile i32 %arg7, ptr addrspace(1) poison

  store volatile i32 %arg8, ptr addrspace(1) poison
  store volatile i32 %arg9, ptr addrspace(1) poison
  store volatile i32 %arg10, ptr addrspace(1) poison
  store volatile i32 %arg11, ptr addrspace(1) poison
  store volatile i32 %arg12, ptr addrspace(1) poison
  store volatile i32 %arg13, ptr addrspace(1) poison
  store volatile i32 %arg14, ptr addrspace(1) poison
  store volatile i32 %arg15, ptr addrspace(1) poison

  store volatile i32 %arg16, ptr addrspace(1) poison
  store volatile i32 %arg17, ptr addrspace(1) poison
  store volatile i32 %arg18, ptr addrspace(1) poison
  store volatile i32 %arg19, ptr addrspace(1) poison
  store volatile i32 %arg20, ptr addrspace(1) poison
  store volatile i32 %arg21, ptr addrspace(1) poison
  store volatile i32 %arg22, ptr addrspace(1) poison
  store volatile i32 %arg23, ptr addrspace(1) poison

  store volatile i32 %arg24, ptr addrspace(1) poison
  store volatile i32 %arg25, ptr addrspace(1) poison
  store volatile i32 %arg26, ptr addrspace(1) poison
  store volatile i32 %arg27, ptr addrspace(1) poison
  store volatile i32 %arg28, ptr addrspace(1) poison
  store volatile i32 %arg29, ptr addrspace(1) poison
  store volatile i32 %arg30, ptr addrspace(1) poison
  store volatile i32 %arg31, ptr addrspace(1) poison
  %private = load volatile i32, ptr addrspace(5) %arg32
  ret void
}

; sp[0] = stack passed %arg31
; sp[1] = byval

; GCN-LABEL: {{^}}kern_call_too_many_args_use_workitem_id_x_byval:

; Local stack object initialize. Offset 0 is the emergency spill slot.
; GCN-DAG: v_mov_b32_e32 [[K:v[0-9]+]], 0x3e7{{$}}
; GCN-DAG: s_movk_i32 s32, 0x400
; GCN: buffer_store_dword [[K]], off, s[0:3], 0

; Pass %arg31 on stack
; GCN: v_mov_b32_e32 [[K1:v[0-9]+]], 0x140{{$}}
; GCN: buffer_store_dword [[K1:v[0-9]+]], off, s[0:3], s32{{$}}

; GCN: buffer_load_dword [[RELOAD_BYVAL:v[0-9]+]], off, s[0:3], 0
; GCN: buffer_store_dword [[RELOAD_BYVAL]], off, s[0:3], s32 offset:4{{$}}
; GCN: v_mov_b32_e32 [[RELOAD_BYVAL]],
; GCN: s_swappc_b64

; GCN: .amdhsa_system_vgpr_workitem_id 0
define amdgpu_kernel void @kern_call_too_many_args_use_workitem_id_x_byval() #1 {
  %alloca = alloca i32, align 4, addrspace(5)
  store volatile i32 999, ptr addrspace(5) %alloca
  call void @too_many_args_use_workitem_id_x_byval(
    i32 10, i32 20, i32 30, i32 40,
    i32 50, i32 60, i32 70, i32 80,
    i32 90, i32 100, i32 110, i32 120,
    i32 130, i32 140, i32 150, i32 160,
    i32 170, i32 180, i32 190, i32 200,
    i32 210, i32 220, i32 230, i32 240,
    i32 250, i32 260, i32 270, i32 280,
    i32 290, i32 300, i32 310, i32 320,
    ptr addrspace(5) byval(i32) %alloca)
  ret void
}

; GCN-LABEL: {{^}}func_call_too_many_args_use_workitem_id_x_byval:
; GCN: v_mov_b32_e32 [[K:v[0-9]+]], 0x3e7{{$}}
; GFX7: buffer_store_dword [[K]], off, s[0:3], s33{{$}}
; GFX90A: buffer_store_dword [[K]], off, s[0:3], s33{{$}}
; GCN: buffer_load_dword [[RELOAD_BYVAL:v[0-9]+]], off, s[0:3], s33{{$}}
; GCN: buffer_store_dword [[RELOAD_BYVAL]], off, s[0:3], s32 offset:4{{$}}
; GCN: v_mov_b32_e32 [[RELOAD_BYVAL]],
; GCN: s_swappc_b64
define void @func_call_too_many_args_use_workitem_id_x_byval() #1 {
  %alloca = alloca i32, align 4, addrspace(5)
  store volatile i32 999, ptr addrspace(5) %alloca
  call void @too_many_args_use_workitem_id_x_byval(
    i32 10, i32 20, i32 30, i32 40,
    i32 50, i32 60, i32 70, i32 80,
    i32 90, i32 100, i32 110, i32 120,
    i32 130, i32 140, i32 150, i32 160,
    i32 170, i32 180, i32 190, i32 200,
    i32 210, i32 220, i32 230, i32 240,
    i32 250, i32 260, i32 270, i32 280,
    i32 290, i32 300, i32 310, i32 320,
    ptr addrspace(5) byval(i32) %alloca)
  ret void
}

; GCN-LABEL: {{^}}too_many_args_use_workitem_id_xyz:
; GFX90A: buffer_load_dword [[LOAD_ARG31:v[0-9]+]], off, s[0:3], s32{{$}}
; GFX90A: v_and_b32_e32 [[ID_X:v[0-9]+]], 0x3ff, v31
; GFX90A: global_store_dword v{{\[[0-9]+:[0-9]+]}}, [[ID_X]], off{{$}}
; GFX90A: v_bfe_u32 [[ID_Y:v[0-9]+]], v31, 10, 10
; GFX90A: v_bfe_u32 [[ID_Z:v[0-9]+]], v31, 20, 10
; GFX90A: global_store_dword v{{\[[0-9]+:[0-9]+]}}, [[ID_Y]], off{{$}}
; GFX90A: global_store_dword v{{\[[0-9]+:[0-9]+]}}, [[ID_Z]], off{{$}}

; GFX7:   v_and_b32_e32 v32, 0x3ff, v31
; GFX7:   v_bfe_u32 v32, v31, 10, 10
; GCN7:   v_bfe_u32 v31, v31, 20, 10
; GFX7:   flat_store_dword v{{\[[0-9]+:[0-9]+]}}, v32{{$}}
; GFX7:   flat_store_dword v{{\[[0-9]+:[0-9]+]}}, v31{{$}}
; GFX7:   buffer_load_dword [[LOAD_ARG31:v[0-9]+]], off, s[0:3], s32{{$}}

; GFX7: flat_store_dword v{{\[[0-9]+:[0-9]+]}}, [[LOAD_ARG31]]
; GFX90A: global_store_dword v{{\[[0-9]+:[0-9]+]}}, [[LOAD_ARG31]]

; GCN-NEXT: s_waitcnt
; GCN-NEXT: s_setpc_b64
define void @too_many_args_use_workitem_id_xyz(
  i32 %arg0, i32 %arg1, i32 %arg2, i32 %arg3, i32 %arg4, i32 %arg5, i32 %arg6, i32 %arg7,
  i32 %arg8, i32 %arg9, i32 %arg10, i32 %arg11, i32 %arg12, i32 %arg13, i32 %arg14, i32 %arg15,
  i32 %arg16, i32 %arg17, i32 %arg18, i32 %arg19, i32 %arg20, i32 %arg21, i32 %arg22, i32 %arg23,
  i32 %arg24, i32 %arg25, i32 %arg26, i32 %arg27, i32 %arg28, i32 %arg29, i32 %arg30, i32 %arg31) #1 {
  %val0 = call i32 @llvm.amdgcn.workitem.id.x()
  store volatile i32 %val0, ptr addrspace(1) poison
  %val1 = call i32 @llvm.amdgcn.workitem.id.y()
  store volatile i32 %val1, ptr addrspace(1) poison
  %val2 = call i32 @llvm.amdgcn.workitem.id.z()
  store volatile i32 %val2, ptr addrspace(1) poison

  store volatile i32 %arg0, ptr addrspace(1) poison
  store volatile i32 %arg1, ptr addrspace(1) poison
  store volatile i32 %arg2, ptr addrspace(1) poison
  store volatile i32 %arg3, ptr addrspace(1) poison
  store volatile i32 %arg4, ptr addrspace(1) poison
  store volatile i32 %arg5, ptr addrspace(1) poison
  store volatile i32 %arg6, ptr addrspace(1) poison
  store volatile i32 %arg7, ptr addrspace(1) poison

  store volatile i32 %arg8, ptr addrspace(1) poison
  store volatile i32 %arg9, ptr addrspace(1) poison
  store volatile i32 %arg10, ptr addrspace(1) poison
  store volatile i32 %arg11, ptr addrspace(1) poison
  store volatile i32 %arg12, ptr addrspace(1) poison
  store volatile i32 %arg13, ptr addrspace(1) poison
  store volatile i32 %arg14, ptr addrspace(1) poison
  store volatile i32 %arg15, ptr addrspace(1) poison

  store volatile i32 %arg16, ptr addrspace(1) poison
  store volatile i32 %arg17, ptr addrspace(1) poison
  store volatile i32 %arg18, ptr addrspace(1) poison
  store volatile i32 %arg19, ptr addrspace(1) poison
  store volatile i32 %arg20, ptr addrspace(1) poison
  store volatile i32 %arg21, ptr addrspace(1) poison
  store volatile i32 %arg22, ptr addrspace(1) poison
  store volatile i32 %arg23, ptr addrspace(1) poison

  store volatile i32 %arg24, ptr addrspace(1) poison
  store volatile i32 %arg25, ptr addrspace(1) poison
  store volatile i32 %arg26, ptr addrspace(1) poison
  store volatile i32 %arg27, ptr addrspace(1) poison
  store volatile i32 %arg28, ptr addrspace(1) poison
  store volatile i32 %arg29, ptr addrspace(1) poison
  store volatile i32 %arg30, ptr addrspace(1) poison
  store volatile i32 %arg31, ptr addrspace(1) poison

  ret void
}

; frame[0] = ID { Z, Y, X }

; GCN-LABEL: {{^}}kern_call_too_many_args_use_workitem_id_xyz:

; GCN-DAG: s_mov_b32 s32, 0

; UNPACKED-TID-DAG: v_lshlrev_b32_e32 v1, 10, v1
; UNPACKED-TID-DAG: v_or_b32_e32 v0, v0, v1
; UNPACKED-TID-DAG: v_lshlrev_b32_e32 v2, 20, v2
; UNPACKED-TID-DAG: v_or_b32_e32 v31, v0, v2
; PACKED-TID-NOT: v0
; PACKED-TID-NOT: v1
; PACKED-TID-NOT: v2
; GCN-DAG: v_mov_b32_e32 [[K:v[0-9]+]], 0x140
; GCN-DAG: buffer_store_dword [[K]], off, s[0:3], s32{{$}}
; GCN: s_swappc_b64

; GCN: .amdhsa_system_vgpr_workitem_id 2
define amdgpu_kernel void @kern_call_too_many_args_use_workitem_id_xyz() #1 {
  call void @too_many_args_use_workitem_id_xyz(
    i32 10, i32 20, i32 30, i32 40,
    i32 50, i32 60, i32 70, i32 80,
    i32 90, i32 100, i32 110, i32 120,
    i32 130, i32 140, i32 150, i32 160,
    i32 170, i32 180, i32 190, i32 200,
    i32 210, i32 220, i32 230, i32 240,
    i32 250, i32 260, i32 270, i32 280,
    i32 290, i32 300, i32 310, i32 320)
  ret void
}

; workitem ID X in register, yz on stack
; v31 = workitem ID X
; frame[0] = workitem { Z, Y, X }

; GCN-LABEL: {{^}}too_many_args_use_workitem_id_x_stack_yz:
; GCN-DAG: v_and_b32_e32 [[IDX:v[0-9]+]], 0x3ff, v31
; GCN-DAG: {{flat|global}}_store_dword v[0:1], [[IDX]]
; GCN-DAG: v_bfe_u32 [[IDY:v[0-9]+]], v31, 10, 10
; GCN-DAG: {{flat|global}}_store_dword v{{\[[0-9]+:[0-9]+]}}, [[IDY]]
; GCN-DAG: v_bfe_u32 [[IDZ:v[0-9]+]], v31, 20, 10
; GCN-DAG: {{flat|global}}_store_dword v{{\[[0-9]+:[0-9]+]}}, [[IDZ]]

; GCN-COUNT-31: {{flat|global}}_store_dword v{{\[[0-9]+:[0-9]+]}}
; GCN-NEXT: s_waitcnt
; GCN: s_setpc_b64
; GCN: ScratchSize: 0
define void @too_many_args_use_workitem_id_x_stack_yz(
  i32 %arg0, i32 %arg1, i32 %arg2, i32 %arg3, i32 %arg4, i32 %arg5, i32 %arg6, i32 %arg7,
  i32 %arg8, i32 %arg9, i32 %arg10, i32 %arg11, i32 %arg12, i32 %arg13, i32 %arg14, i32 %arg15,
  i32 %arg16, i32 %arg17, i32 %arg18, i32 %arg19, i32 %arg20, i32 %arg21, i32 %arg22, i32 %arg23,
  i32 %arg24, i32 %arg25, i32 %arg26, i32 %arg27, i32 %arg28, i32 %arg29, i32 %arg30) #1 {
  %val0 = call i32 @llvm.amdgcn.workitem.id.x()
  store volatile i32 %val0, ptr addrspace(1) poison
  %val1 = call i32 @llvm.amdgcn.workitem.id.y()
  store volatile i32 %val1, ptr addrspace(1) poison
  %val2 = call i32 @llvm.amdgcn.workitem.id.z()
  store volatile i32 %val2, ptr addrspace(1) poison

  store volatile i32 %arg0, ptr addrspace(1) poison
  store volatile i32 %arg1, ptr addrspace(1) poison
  store volatile i32 %arg2, ptr addrspace(1) poison
  store volatile i32 %arg3, ptr addrspace(1) poison
  store volatile i32 %arg4, ptr addrspace(1) poison
  store volatile i32 %arg5, ptr addrspace(1) poison
  store volatile i32 %arg6, ptr addrspace(1) poison
  store volatile i32 %arg7, ptr addrspace(1) poison

  store volatile i32 %arg8, ptr addrspace(1) poison
  store volatile i32 %arg9, ptr addrspace(1) poison
  store volatile i32 %arg10, ptr addrspace(1) poison
  store volatile i32 %arg11, ptr addrspace(1) poison
  store volatile i32 %arg12, ptr addrspace(1) poison
  store volatile i32 %arg13, ptr addrspace(1) poison
  store volatile i32 %arg14, ptr addrspace(1) poison
  store volatile i32 %arg15, ptr addrspace(1) poison

  store volatile i32 %arg16, ptr addrspace(1) poison
  store volatile i32 %arg17, ptr addrspace(1) poison
  store volatile i32 %arg18, ptr addrspace(1) poison
  store volatile i32 %arg19, ptr addrspace(1) poison
  store volatile i32 %arg20, ptr addrspace(1) poison
  store volatile i32 %arg21, ptr addrspace(1) poison
  store volatile i32 %arg22, ptr addrspace(1) poison
  store volatile i32 %arg23, ptr addrspace(1) poison

  store volatile i32 %arg24, ptr addrspace(1) poison
  store volatile i32 %arg25, ptr addrspace(1) poison
  store volatile i32 %arg26, ptr addrspace(1) poison
  store volatile i32 %arg27, ptr addrspace(1) poison
  store volatile i32 %arg28, ptr addrspace(1) poison
  store volatile i32 %arg29, ptr addrspace(1) poison
  store volatile i32 %arg30, ptr addrspace(1) poison

  ret void
}

; GCN-LABEL: {{^}}kern_call_too_many_args_use_workitem_id_x_stack_yz:

; GCN-NOT: v0
; UNPACKED-TID-DAG: v_lshlrev_b32_e32 v1, 10, v1
; UNPACKED-TID-DAG: v_or_b32_e32 v0, v0, v1
; UNPACKED-TID-DAG: v_lshlrev_b32_e32 v2, 20, v2
; UNPACKED-TID-DAG: v_or_b32_e32 v31, v0, v2
; PACKED-TID: v_mov_b32_e32 v31, v0

; GCN: s_mov_b32 s32, 0
; GCN: s_swappc_b64

; GCN: .amdhsa_system_vgpr_workitem_id 2
define amdgpu_kernel void @kern_call_too_many_args_use_workitem_id_x_stack_yz() #1 {
  call void @too_many_args_use_workitem_id_x_stack_yz(
    i32 10, i32 20, i32 30, i32 40,
    i32 50, i32 60, i32 70, i32 80,
    i32 90, i32 100, i32 110, i32 120,
    i32 130, i32 140, i32 150, i32 160,
    i32 170, i32 180, i32 190, i32 200,
    i32 210, i32 220, i32 230, i32 240,
    i32 250, i32 260, i32 270, i32 280,
    i32 290, i32 300, i32 310)
  ret void
}

declare i32 @llvm.amdgcn.workitem.id.x() #0
declare i32 @llvm.amdgcn.workitem.id.y() #0
declare i32 @llvm.amdgcn.workitem.id.z() #0

attributes #0 = { nounwind readnone speculatable "amdgpu-flat-work-group-size"="1,512" }
attributes #1 = { nounwind noinline "amdgpu-flat-work-group-size"="1,512" }
