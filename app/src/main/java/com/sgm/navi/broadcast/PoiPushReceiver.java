package com.sgm.navi.broadcast;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;

import com.android.utils.log.Logger;
import com.android.utils.process.ProcessManager;
import com.sgm.navi.exportservice.ExportIntentParam;
import com.sgm.navi.hmi.launcher.FloatViewManager;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.vrbridge.IVrBridgeConstant;

import org.json.JSONException;
import org.json.JSONObject;

public class PoiPushReceiver extends BroadcastReceiver {

    /**
     * 品牌App发送Poi到车，数据结构eg:
     * {
     * 	"content": "{\"endPoint\":{\"addr\":\"新昌县泰坦大道中国茶市1幢1楼（近新昌大佛寺风景区）\",\"coordType\":\"gcj0211\",\"name\":\"\"},\"prefer\":0}",
     * 	"extra": {},
     * 	"id": "37645493",
     * 	"timeStamp": "1752029388531",
     * 	"version": "0.0.1"
     * }
     */
    private static final String TAG = PoiPushReceiver.class.getSimpleName();
    //action
    public static final String PUSH_ACTION = "patac.intent.action.push.RECEIVE";
    //key
    private static final String PUSH_DATA = "patac_push_data";
    private static final String KEY_CONTENT = "content";
    private static final String KEY_END_POINT = "endPoint";
    private static final String KEY_ADDRESS = "addr";


    @Override
    public void onReceive(Context context, Intent intent) {
        if (null != intent && PUSH_ACTION.equals(intent.getAction())) {
            final String pushData = intent.getStringExtra(PUSH_DATA);
            if (Logger.openLog) {
                Logger.i(TAG, "receive pushData: " + pushData);
            }
            try {
                JSONObject pushObj = new JSONObject(pushData);
                JSONObject contentObj = pushObj.getJSONObject(KEY_CONTENT);
                JSONObject endPoint = contentObj.getJSONObject(KEY_END_POINT);
                String address = endPoint.getString(KEY_ADDRESS);
                if (!TextUtils.isEmpty(address)) {
                    final boolean appForeGroundStatus = ProcessManager.isAppInForeground();
                    if (appForeGroundStatus) {
                        //App已经打开 ，Package回调打开对应界面
                        final Bundle bundle = new Bundle();
                        bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.KEYWORD_SEARCH);
                        bundle.putString(IVrBridgeConstant.VoiceIntentParams.KEYWORD, address);
                        MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
                    } else {
                        //App未打开状态，打开地图并通过ExportIntentParam保存数据
                        ExportIntentParam.setIntentPage(IVrBridgeConstant.VoiceIntentPage.KEYWORD_SEARCH);
                        ExportIntentParam.setKeyword(address);
                        AppCache.getInstance().openMap(FloatViewManager.getInstance().isNaviDeskBg());
                    }
                }
            } catch (JSONException exception) {
                Logger.w(TAG, "parse poiPushMsg error:", exception.getMessage());
            }
        }
    }

}
