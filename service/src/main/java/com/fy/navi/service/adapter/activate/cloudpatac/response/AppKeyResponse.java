package com.fy.navi.service.adapter.activate.cloudpatac.response;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;
import com.fy.navi.service.MapDefaultFinalTag;

import org.json.JSONException;

public class AppKeyResponse extends BaseResponse {

    @NonNull
    @Override
    public String toString() {
        return "AppKeyResponse{" +
                "resultCode='" + getMResultCode() + '\'' +
                ", message='" + getMMessage() + '\'' +
                ", appKey = '" + getAppKey() + '\'' +
                '}';
    }
    /**
     * 获取appKey
     * @return appKey
     */
    public String getAppKey() {
        String appKey = "";
        try{
            appKey = getMDataSet().getString("appKey");
        } catch (JSONException e) {
            Logger.e(MapDefaultFinalTag.ACTIVATE_SERVICE_TAG, e.toString());
        }
        return appKey;
    }


}
