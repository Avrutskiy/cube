# CubeDiff Rendering Algorithm

This repository contains the implementation of a simple differentiable rendering algorithm in Mathematica, designed to generate data used in the **CubeDiff** dataset. The algorithm renders images of a cube in various orientations and computes their first- and second-order derivatives with respect to the cube's six degrees of freedom (three translations and three rotations).

### Exported Data

For each batch number `KA`, the algorithm generates the following data files:

- **Cube images:** `img-KA.mx` – 41 by 41 images of the cube.
- **First derivatives of images:** `d_img-KA.mx` – Derivatives of the images with respect to the cube’s six degrees of freedom.
- **Second derivatives of images:** `d2_img-KA.mx` – Second derivatives of the images with respect to pairs of degrees of freedom.
- **3D coordinates of vertices:** `3d-KA.mx` – The x, y, and z coordinates of three vertices of the cube, which uniquely define its orientation.
- **First derivatives of vertex coordinates:** `d_3d-KA.mx` – Derivatives of the 3D vertex coordinates.
- **Second derivatives of vertex coordinates:** `d2_3d-KA.mx` – Second-order derivatives of the 3D vertex coordinates.

### Cube Orientation Sequence

The cube’s orientations are defined by a randomized sequence stored in the file `6d100k.mx`.

### Overview

The goal of this algorithm is to generate both images and their derivatives, which can be used for studying how incorporating higher-order derivatives can improve the accuracy of image analysis tasks.

For further details on the algorithm and its application, please refer to the appendix of [arXiv:2310.14045](https://arxiv.org/abs/2310.14045).
