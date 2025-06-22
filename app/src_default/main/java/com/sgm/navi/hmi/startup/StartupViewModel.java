package com.sgm.navi.hmi.startup;

import android.app.Application;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class StartupViewModel extends BaseStartupViewModel {

    public StartupViewModel(@NonNull Application application) {
        super(application);
        Logger.i(TAG, "我是默认车型");
    }
}