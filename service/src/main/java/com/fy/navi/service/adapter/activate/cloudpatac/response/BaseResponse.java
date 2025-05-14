package com.fy.navi.service.adapter.activate.cloudpatac.response;

import com.google.gson.annotations.SerializedName;

import org.json.JSONObject;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class BaseResponse {
    @SerializedName("resultCode")
    private String mResultCode;
    @SerializedName("message")
    private String mMessage;
    @SerializedName("dataSet")
    private JSONObject mDataSet;

}
