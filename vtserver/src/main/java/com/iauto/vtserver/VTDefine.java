package com.iauto.vtserver;

public class VTDefine {
    // notify eventtype
    public static final int EN_VT_INITIALIZED       = 0x1000;
    public static final int EN_VT_REMOTE_CLOSED      = 0x1010;

    // notify code type
    public static final int VT_SUCCESS = 0x00;
    public static final int VT_FAILURE = 0x01;
    public static final int VT_SRC_NOT_AVL = 0x02;
    public static final int VT_CLI_CONNECT_LOST = 0x0D;       // client connection lost
}