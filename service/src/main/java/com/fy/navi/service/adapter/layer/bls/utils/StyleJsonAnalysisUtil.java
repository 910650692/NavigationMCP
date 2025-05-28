package com.fy.navi.service.adapter.layer.bls.utils;

import android.text.TextUtils;

import com.android.utils.log.Logger;
import com.fy.navi.service.adapter.layer.bls.bean.MarkerInfoBean;
import com.google.gson.Gson;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * 图层json解析util类, 以便其它位置使用
 * Created by AutoSdk on 2020/9/3.
 */
public class StyleJsonAnalysisUtil {
    private  final String TAG = "JsonUtil";
    private  Gson mGson;
    private  JSONObject mObjJsonBean;
    private  JSONObject mObjStyleJsonBean;
    private  JSONObject mObjMarkerJsonBean;
    private  JSONObject mObjDemoJsonBean;

    public Gson getGson() {
        if (mGson == null) {
            mGson = new Gson();
        }
        return mGson;
    }

    public StyleJsonAnalysisUtil(String fileStringJson){
        try{
            if (mObjJsonBean == null) {
                mObjJsonBean = new JSONObject(fileStringJson);
            }

            mObjStyleJsonBean = mObjJsonBean.getJSONObject("layer_item_info");
            mObjMarkerJsonBean = mObjJsonBean.getJSONObject("marker_info");
            mObjDemoJsonBean = mObjJsonBean.getJSONObject("demo_info");

        } catch (JSONException e) {

        }
    }

    /**
     * @param key isIncludeKey true:拼凑json包含外层为key，false只拼凑value的json
     */
    public String getStyleBeanJson(String key) {
        StringBuilder jsonBeanBuild = new StringBuilder();
        try {
            jsonBeanBuild.append(mObjStyleJsonBean.get(key).toString());
        } catch (JSONException e) {
            Logger.e(TAG, "解析出错, key:" + key, e);

        }

        return jsonBeanBuild.toString();
    }

    public String getStyleBeanJsonWithNightMode(String key, boolean nightMode) {
        StringBuilder jsonBeanBuild = new StringBuilder();
        try {
            Object object = mObjStyleJsonBean.get(key);
            if (nightMode && object == null) {
                object = mObjStyleJsonBean.get(key + "_night");
            }
            jsonBeanBuild.append(object.toString());
        } catch (JSONException e) {
            Logger.e(TAG, "解析出错, key:" + key, e);

        }

        return jsonBeanBuild.toString();
    }

    public String getDemoStyleBeanJsonWithNightMode(String key, boolean nightMode) {
        StringBuilder jsonBeanBuild = new StringBuilder();
        try {
            Object object = mObjDemoJsonBean.get(key);
            if (nightMode && object == null) {
                object = mObjDemoJsonBean.get(key + "_night");
            }
            jsonBeanBuild.append(object.toString());
        } catch (JSONException e) {
            Logger.e(TAG, "解析出错, key:" + key, e);

        }

        return jsonBeanBuild.toString();
    }

    public String formatJsonStr(String json) {
        if (json == null || json.length() == 0) {
            return "";
        }

        String message = json;
        try {
            json = json.trim();
            if (json.startsWith("{")) {
                JSONObject jsonObject = new JSONObject(json);
                message = jsonObject.toString(2);
                message = message.replaceAll("\n", "\n║ ");
            } else if (json.startsWith("[")) {
                JSONArray jsonArray = new JSONArray(json);
                message = jsonArray.toString(2);
                message = message.replaceAll("\n", "\n║ ");
            }
        } catch (Exception e) {

        }
        return message;
    }


    /**
     * 从夫json中获取子json数据
     *
     * @param parent
     * @param key
     * @return
     */
    public String getStyleBeanFromParent(String parent, String key) {
        String json = getStyleBeanJson(parent);
        StringBuilder jsonBeanBuild = new StringBuilder();
        JSONObject obj = null;
        try {
            obj = new JSONObject(json);
            jsonBeanBuild.append(obj.get(key).toString());
        } catch (JSONException e) {
            Logger.e(TAG, "解析出错2", e);

        }
        Logger.d(TAG, "getStyleBeanJsonWithKey: " + key + ":" + jsonBeanBuild);
        return jsonBeanBuild.toString();
    }


    /**
     * 传入现有allJson返回指定key的json
     *
     * @param allJson
     * @return
     */
    public String getBeanJson(String allJson, String key) {
        StringBuilder jsonBeanBuild = new StringBuilder();
        JSONObject obj = null;
        try {
            obj = new JSONObject(allJson);
            jsonBeanBuild.append(obj.get(key).toString());
        } catch (JSONException e) {
            Logger.e(TAG, "解析出错3", e);

        }

        Logger.d(TAG, "getBeanJson，allJson = : " + allJson);
        Logger.d(TAG, "getBeanJson，json = : " + jsonBeanBuild);
        return jsonBeanBuild.toString();
    }



    /**
     * @param strMarkerInfo 最外层markerinfo名称，通常为marker_info
     */
    public MarkerInfoBean getMarkerInfoFromJson(String strMarkerInfo) {
        MarkerInfoBean markerInfoBean = null;
        if (TextUtils.isEmpty(strMarkerInfo)) {
            return null;
        }

//        Logger.d(TAG, "getMarkerInfoFromJson:strMarkerInfo =  " + strMarkerInfo);

        try {
            markerInfoBean = getGson().fromJson(mObjMarkerJsonBean.getJSONObject(strMarkerInfo)
                    .toString(), MarkerInfoBean.class);
        } catch (JSONException e) {


        }

//        Logger.d(TAG, "getMarkerInfoFromJson: info" + getGson().toJson(markerInfoBean));

        return markerInfoBean;
    }

}