package com.sgm.navi.broadcast;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;

import com.android.utils.log.Logger;
import com.android.utils.process.ProcessManager;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.google.gson.JsonParser;
import com.sgm.navi.hmi.launcher.FloatViewManager;
import com.sgm.navi.mapservice.bean.INaviConstant;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.utils.ExportIntentParam;
import com.sgm.navi.vrbridge.IVrBridgeConstant;


public class PoiPushReceiver extends BroadcastReceiver {

    /**
     * 品牌App发送Poi到车，数据结构eg:
     * name或addr至少一个有效才能正常发起搜索
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
    private static final String KEY_NAME = "name";


    @Override
    public void onReceive(Context context, Intent intent) {
        if (null != intent && PUSH_ACTION.equals(intent.getAction())) {
            final String pushData = intent.getStringExtra(PUSH_DATA);
            if (Logger.openLog) {
                Logger.i(TAG, "receive pushData: " + pushData);
            }
            if (TextUtils.isEmpty(pushData)) {
                Logger.e(TAG, "pushData is empty");
                return;
            }
            try {
                JsonObject rootObj = JsonParser.parseString(pushData).getAsJsonObject();
                String contentStr = rootObj.getAsJsonPrimitive(KEY_CONTENT).getAsString();
                JsonObject contentObj = JsonParser.parseString(contentStr).getAsJsonObject();
                JsonObject endPointObj = contentObj.getAsJsonObject(KEY_END_POINT);
                String address = endPointObj.getAsJsonPrimitive(KEY_ADDRESS).getAsString();
                String name = endPointObj.getAsJsonPrimitive(KEY_NAME).getAsString();
                String keyword;
                if (!TextUtils.isEmpty(name)) {
                    keyword = name;
                } else if (!TextUtils.isEmpty(address)) {
                    keyword = address;
                } else {
                    Logger.e(TAG, "keyword is empty");
                    return;
                }

                final boolean appForeGroundStatus = ProcessManager.isAppInForeground();
                if (appForeGroundStatus) {
                    //App已经打开 ，Package回调打开对应界面
                    final Bundle bundle = new Bundle();
                    bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.KEYWORD_SEARCH);
                    bundle.putString(IVrBridgeConstant.VoiceIntentParams.KEYWORD, keyword);
                    MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
                } else {
                    //App未打开状态，打开地图并通过ExportIntentParam保存数据
                    ExportIntentParam.setIntentPage(INaviConstant.OpenIntentPage.SEARCH_PAGE);
                    ExportIntentParam.setKeyword(keyword);
                    ProcessManager.restartProcess(context.getApplicationContext(), FloatViewManager.getInstance().isNaviDeskBg());
                }
            } catch (JsonParseException exception) {
                Logger.w(TAG, "parse poiPushMsg error:", exception.getMessage());
            }
        }
    }

}
