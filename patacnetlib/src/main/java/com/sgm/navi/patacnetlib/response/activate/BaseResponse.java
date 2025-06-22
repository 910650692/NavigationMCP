package com.sgm.navi.patacnetlib.response.activate;

import com.google.gson.annotations.SerializedName;

import lombok.Getter;

@Getter
public class BaseResponse {
    @SerializedName("resultCode")
    private String mResultCode;
    @SerializedName("message")
    private String mMessage;

}
