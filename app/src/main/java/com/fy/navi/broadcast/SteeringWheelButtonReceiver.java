package com.fy.navi.broadcast;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.util.Log;

/**
 * 语言
 */
public class SteeringWheelButtonReceiver extends BroadcastReceiver {
    private static final String TAG = SteeringWheelButtonReceiver.class.getSimpleName();
    public static final String ACTION = "com.fy.navi.broadcast.SteeringWheelButtonReceiver";

    @Override
    public void onReceive(Context context, Intent intent) {
        if (!ACTION.equals(intent.getAction())) {
            return;
        }
        Log.d(TAG, "onReceive: ");
        //播报导航上一条
        // TODO
    }
}
