package com.fy.navi.ui.view.refresh;

import androidx.annotation.IntDef;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

public class State {

    @IntDef({RefreshState.REFRESH,
            RefreshState.LOADMORE
    })
    @Retention(RetentionPolicy.SOURCE)
    public @interface RefreshState {
        int REFRESH = 10;
        int LOADMORE = 11;
    }
}
