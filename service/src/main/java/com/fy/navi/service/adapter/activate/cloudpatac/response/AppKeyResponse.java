package com.fy.navi.service.adapter.activate.cloudpatac.response;

import com.patac.netlib.bean.BaseResponse;

import org.json.JSONObject;

public class AppKeyResponse extends BaseResponse<JSONObject> {

    @Override
    public String toString() {
        return "AppKeyResponse{" +
                "resultCode='" + getResultCode() + '\'' +
                ", message='" + getMessage() + '\'' +
                '}';
    }


}
