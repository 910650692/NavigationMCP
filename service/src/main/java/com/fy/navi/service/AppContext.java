package com.fy.navi.service;

import android.content.Context;

import com.fy.navi.ui.BaseApplication;

import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
public class AppContext {
    private BaseApplication mApplication;
    private Context mContext;

    public static AppContext getInstance() {
        return AppContext.Helper.RA;
    }

    private static final class Helper {
        private static final AppContext RA = new AppContext();
    }
}
