package com.fy.navi.hud;

import android.os.Binder;

public abstract class VTBinder extends Binder {
    public abstract void init();

    public abstract void start();

    public abstract void stop();

    public abstract void uninit();

    public abstract void notifyError();
}
