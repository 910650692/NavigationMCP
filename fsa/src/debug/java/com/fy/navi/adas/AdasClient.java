package com.fy.navi.adas;

import android.content.Context;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;

public final class AdasClient {
    private static final String TAG = AdasClient.class.getSimpleName();

    public static AdasClient getInstance() {
        return SingleHolder.INSTANCE;
    }

    private final static class SingleHolder {
        private static final AdasClient INSTANCE = new AdasClient();
    }

    private AdasClient() {
    }

    public void init(@NonNull final Context context) {
        Logger.e(TAG,"AdasClient start error");
    }
}
