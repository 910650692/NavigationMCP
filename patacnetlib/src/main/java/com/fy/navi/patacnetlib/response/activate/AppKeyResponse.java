package com.fy.navi.patacnetlib.response.activate;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;
import com.fy.navi.patacnetlib.api.NetApiHelper;

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
            Logger.e(NetApiHelper.ACTIVATE_TAG, e.toString());
        }
        return appKey;
    }


}
