#include <stdio.h>
#include "libsd.h"

#include "spu2regs.h"

/*!
 * @file libsd.cpp
 * Stub implementation of the IOP sound library
 */

namespace iop {

// u32 BatchData __attribute__((aligned(16)));
volatile u16* ParamRegList[] = {
    SD_VP_VOLL(0, 0), SD_VP_VOLR(0, 0), SD_VP_PITCH(0, 0), SD_VP_ADSR1(0, 0), SD_VP_ADSR2(0, 0),
    SD_VP_ENVX(0, 0), SD_VP_VOLXL(0, 0), SD_VP_VOLXR(0, 0), SD_P_MMIX(0), SD_P_MVOLL(0),
    SD_P_MVOLR(0), SD_P_EVOLL(0), SD_P_EVOLR(0), SD_P_AVOLL(0), SD_P_AVOLR(0), SD_P_BVOLL(0),
    SD_P_BVOLR(0), SD_P_MVOLXL(0), SD_P_MVOLXR(0), SD_S_PMON_HI(0), SD_S_NON_HI(0), SD_A_KON_HI(0),
    SD_A_KOFF_HI(0), SD_S_ENDX_HI(0), SD_S_VMIXL_HI(0), SD_S_VMIXEL_HI(0), SD_S_VMIXR_HI(0),
    SD_S_VMIXER_HI(0), SD_A_ESA_HI(0), SD_A_EEA_HI(0), SD_A_TSA_HI(0), SD_CORE_IRQA(0),
    SD_VA_SSA_HI(0, 0), SD_VA_LSAX(0, 0), SD_VA_NAX(0, 0), SD_CORE_ATTR(0), SD_A_TSA_HI(0),
    SD_A_STD(0),
    // 1AE & 1B0 are both related to core attr & dma somehow
    U16_REGISTER(0x1AE), U16_REGISTER(0x1B0), (u16*)0xBF900334};

void InitSpu2() {
  U32_REGISTER_WRITE(U32_REGISTER(0x1404), 0xBF900000);
  U32_REGISTER_WRITE(U32_REGISTER(0x140C), 0xBF900800);
  U32_REGISTER_WRITEOR(U32_REGISTER(0x10F0), 0x80000);
  U32_REGISTER_WRITEOR(U32_REGISTER(0x1570), 8);
  U32_REGISTER_WRITE(U32_REGISTER(0x1014), 0x200B31E1);
  U32_REGISTER_WRITE(U32_REGISTER(0x1414), 0x200B31E1);
}

s32 _start(char** argv, int argc) {
  // printf("Starting Spu2...");
  // if (RegisterLibraryEntries(&_exp_libsd) != 0)
  //  return 1;
  InitSpu2();
  return 0;
}

u32 sceSdGetAddr(u16 reg) {
  volatile u16* reg1;
  u16 voice;
  u32 retlo, rethi;

  reg1 = ParamRegList[(reg >> 8) & 0xFF] + ((reg & 1) << 9);
  voice = reg & 0x3E;
  reg1 += ((voice << 1) + voice);
  rethi = U16_REGISTER_READ(reg1) << 17;
  retlo = 0x1FFFF;

  reg &= 0xFF00;

  if (reg != 0x1D00) {
    retlo = U16_REGISTER_READ(reg1 + 1) << 1;

    if ((reg == 0x2100) || (reg == 0x2200)) {
      u32 lo, hi;

      hi = U16_REGISTER_READ(reg1) << 17;
      lo = U16_REGISTER_READ(reg1 + 1) << 1;

      if ((rethi == hi) || (retlo != lo)) {
        retlo = lo;
        rethi = hi;
      }
    }
  }

  return rethi | retlo;
}

void sceSdSetAddr(u16 reg, u32 val) {
  volatile u16* reg1;
  u16 voice;

  reg1 = ParamRegList[(reg >> 8) & 0xFF] + ((reg & 1) << 9);
  voice = reg & 0x3E;

  reg1 += ((voice << 1) + voice);

  U16_REGISTER_WRITE(reg1++, (val >> 17) & 0xFFFF);

  if ((reg & 0xFF00) != 0x1D00) {
    U16_REGISTER_WRITE(reg1, (val >> 1) & 0xFFF8);
  }
}

void sceSdSetParam(u16 reg, u16 val) {
  u32 offs;
  u32 voice;
  u32 reg_index;
  u32 core;
  volatile u16* reg_p;

  core = reg & 1;

  // Determine the channel offset
  if (reg & 0x80)
    offs = (40 * core) >> 1;
  else
    offs = (1024 * core) >> 1;

  reg_index = (reg >> 8) & 0xFF;
  voice = (reg & 0x3E) << 2;
  reg_p = ParamRegList[reg_index] + offs + voice;

  U16_REGISTER_WRITE(reg_p, val);
}

u16 sceSdGetParam(u16 reg) {
  u32 offs;
  u32 voice;
  u32 reg_index;
  u32 core;
  volatile u16* reg_p;

  core = reg & 1;

  // Determine the channel offset
  if (reg & 0x80)
    offs = (40 * core) >> 1;
  else
    offs = (1024 * core) >> 1;

  reg_index = (reg >> 8) & 0xFF;
  voice = (reg & 0x3E) << 2;
  reg_p = ParamRegList[reg_index] + offs + voice;

  return U16_REGISTER_READ(reg_p);
}

void sceSdSetSwitch(u16 reg, u32 val) {
  u32 reg_index;
  volatile u16* reg_p;

  reg_index = (reg >> 8) & 0xFF;
  reg_p = ParamRegList[reg_index] + ((reg & 1) << 9);

  U16_REGISTER_WRITE(reg_p + 0, (u16)val);
  U16_REGISTER_WRITE(reg_p + 1, (u16)((val >> 16) & 0xFF));
}

u32 sceSdGetSwitch(u16 reg) {
  u32 reg_index;
  volatile u16* reg_p;
  u32 ret;

  reg_index = (reg >> 8) & 0xFF;
  reg_p = ParamRegList[reg_index] + ((reg & 1) << 9);

  ret = U16_REGISTER_READ(reg_p + 0);
  ret |= U16_REGISTER_READ(reg_p + 1) << 16;

  return ret;
}

int sceSdProcBatch(sceSdBatch* batch, u32* rets, u32 num) {
  s32 loop;
  s32 ret;

  for (loop = 0; loop < num; loop++) {
    ret = 0;

    switch (batch[loop].func) {
      case SD_BATCH_SETPARAM:
        sceSdSetParam(batch[loop].entry, batch[loop].value);
        break;
      case SD_BATCH_GETPARAM:
        ret = sceSdGetParam(batch[loop].entry);
        break;
      case SD_BATCH_SETSWITCH:
        sceSdSetSwitch(batch[loop].entry, batch[loop].value);
        break;
      case SD_BATCH_GETSWITCH:
        ret = sceSdGetSwitch(batch[loop].entry);
        break;
      case SD_BATCH_SETADDR:
        sceSdSetAddr(batch[loop].entry, batch[loop].value);
        break;
      case SD_BATCH_GETADDR:
        ret = sceSdGetAddr(batch[loop].entry);
        break;
      case SD_BATCH_SETCORE:
        // sceSdSetCoreAttr(batch[loop].entry, batch[loop].value);
        break;
      case SD_BATCH_GETCORE:
        // ret = sceSdGetCoreAttr(batch[loop].entry);
        break;
      case SD_BATCH_WRITEIOP:
        *((u32*)batch[loop].value) = batch[loop].entry;
        break;
      case SD_BATCH_WRITEEE:
        // BatchData = batch[loop].entry;
        // SifDmaBatch(batch[loop].value, &BatchData, 4);
        break;
      case SD_BATCH_EERETURN:
        // SifDmaBatch(batch[loop].value, rets, batch[loop].entry);
        break;
      default:
        return -1 - loop;
    }

    if (rets)
      rets[loop] = ret;
  }

  return loop;
}
}  // namespace iop
