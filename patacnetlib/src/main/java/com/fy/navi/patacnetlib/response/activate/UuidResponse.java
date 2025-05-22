package com.fy.navi.patacnetlib.response.activate;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;
import com.fy.navi.patacnetlib.api.NetApiHelper;

import org.json.JSONException;


public class UuidResponse extends BaseResponse {

    /**
     * 获取uuid
     * @return uuid
     */
    public String getUuid() {
        String uuid = "";
        try {
            uuid = getMDataSet().getString("vin");
        } catch (JSONException e) {
            Logger.e(NetApiHelper.ACTIVATE_TAG, e.toString());
        }
        return uuid;
    }

    @NonNull
    @Override
    public String toString() {
        return "UuidResponse{" +
                "resultCode='" + getMResultCode() + '\'' +
                ", message='" + getMMessage() + '\'' +
                ", uuid = '" + getUuid() + '\'' +
                '}';
    }
}
