package com.iauto.vtserver;

public interface IVTServerListener {
    void onNotifyEvent(int eventtype, int code, String msg);
}