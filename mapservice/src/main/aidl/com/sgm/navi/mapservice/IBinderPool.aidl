package com.sgm.navi.mapservice;

import com.sgm.navi.mapservice.IBinderPoolCallback;

interface IBinderPool {

    void addBindPoolCallback(String pckName, IBinderPoolCallback binderPoolCallback);

    boolean getEngineInitStatus(String pckName);

    void startInitEngine(String pkgName);

    IBinder queryBinder(String packName, String binderCode);
}