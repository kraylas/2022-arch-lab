# 基于md5算法的硬件设计实验

## 实验背景
MD5消息摘要算法（英语：MD5 Message-Digest Algorithm），一种被广泛使用的密码散列函数，可以产生出一个128位（16个字符(BYTES)）的散列值（hash value），用于确保信息传输完整一致。MD5由美国密码学家罗纳德·李维斯特（Ronald Linn Rivest）设计，于1992年公开，用以取代MD4算法。这套算法的程序在 RFC 1321 中被加以规范。

## 实验目的
本次实验目标是设计一个可用的计算md5的硬件，为了简化流程和方便实现，本实验对标准md5算法进行了简化，具体流程可以参照附件内的c代码。

## 实验流程
本次实验可选 chisel 实现和 verilog 实现，推荐使用chisel实现，代码更为简洁。

首先需要将附件中的c代码完全理解，然后按照c代码的流程设计硬件。

我们对于两种语言都提供了一个框架，你只需要补全框架中缺失的部分即可。

首先这两种方法都需要在你的linux环境下安装cmake, g++编译环境和verilator
### chisel流程
Chisel相对于Verilog来讲，最大的优势特性有两个：
> 1. 代码复用
> 2. 设计参数化

chisel的安装和使用文档可见[官方文档](https://www.chisel-lang.org/)

本实验只需要安装scala环境即可，附件里的工程sbt文件已经写好了自动配置chisel的部分

推荐使用idea来进行开发
> 1. 目标1:导入附件中的chisel工程，补全计算单个round的部分
> 2. 目标2:导入附件中的chisel工程，完成整个设计

附件中已经写好test，通过test代表实验完成

### verilog流程
首先安装上面的环境，然后补全verilog设计。只需要按照运行cmakelists运行即可运行测试。