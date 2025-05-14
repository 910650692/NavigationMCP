package com.fy.navi.service.adapter.activate.cloudpatac.response;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;
import com.fy.navi.service.MapDefaultFinalTag;
import com.patac.netlib.bean.BaseResponse;

import org.json.JSONException;
import org.json.JSONObject;

public class UuidResponse extends BaseResponse<JSONObject> {

    /**
     * 获取uuid
     * @return uuid
     */
    public String getUuid() {
        String uuid = "";
        try {
            uuid = getDataSet().getString("vin");
        } catch (JSONException e) {
            Logger.e(MapDefaultFinalTag.ACTIVATE_SERVICE_TAG, e.toString());
        }
        return uuid;
    }

    @NonNull
    @Override
    public String toString() {
        return "UuidResponse{" +
                "resultCode='" + getResultCode() + '\'' +
                ", message='" + getMessage() + '\'' +
                ", uuid = '" + getUuid() + '\'' +
                '}';
    }
}
