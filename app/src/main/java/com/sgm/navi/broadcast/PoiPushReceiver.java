package com.sgm.navi.broadcast;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

import com.android.utils.log.Logger;

public class PoiPushReceiver extends BroadcastReceiver {

    private static final String TAG = PoiPushReceiver.class.getSimpleName();
    //action
    public static final String PUSH_ACTION = "patac.intent.action.push.RECEIVE";
    //key
    private static final String PUSH_DATA = "patac_push_data";

    @Override
    public void onReceive(Context context, Intent intent) {
        Logger.i(TAG, "receive broadcast");
        if (null != intent && PUSH_ACTION.equals(intent.getAction())) {
            final String pushData = intent.getStringExtra(PUSH_DATA);
            if (Logger.openLog) {
                Logger.i(TAG, "receive pushData: " + pushData);
            }
        }
    }

}
