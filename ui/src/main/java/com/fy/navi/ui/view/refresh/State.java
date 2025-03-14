package com.fy.navi.ui.view.refresh;

import androidx.annotation.IntDef;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

public class State {

    @IntDef({REFRESH_STATE.REFRESH,
            REFRESH_STATE.LOADMORE
    })
    @Retention(RetentionPolicy.SOURCE)
    public @interface REFRESH_STATE {
        int REFRESH = 10;
        int LOADMORE = 11;
    }
}
