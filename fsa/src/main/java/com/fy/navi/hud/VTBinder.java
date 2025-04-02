package com.fy.navi.hud;

import android.os.Binder;

public abstract class VTBinder extends Binder {
    /**
     * 初始化
     */
    public abstract void init();

    /**
     * 启动
     */
    public abstract void start();

    /**
     * 停止
     */
    public abstract void stop();

    /**
     * 销毁
     */
    public abstract void uninit();

    /**
     * 通知错误
     */
    public abstract void notifyError();

    /**
     * 获取路口大图
     * @return byte[]
     */
    public abstract byte[] getCrossImg();
}
