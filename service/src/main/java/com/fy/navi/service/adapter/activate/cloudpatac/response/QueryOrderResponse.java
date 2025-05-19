package com.fy.navi.service.adapter.activate.cloudpatac.response;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;
import com.fy.navi.service.MapDefaultFinalTag;
import com.patac.netlib.bean.BaseResponse;

import org.json.JSONException;
import org.json.JSONObject;

public class QueryOrderResponse extends BaseResponse<JSONObject> {

    /**
     * 获取订单号
     *
     * @return 订单号
     */
    public String getCusOrderId() {
        String orderId = "";
        try {
            orderId = getDataSet().getString("cusOrderId");
        } catch (JSONException e) {
            Logger.e(MapDefaultFinalTag.ACTIVATE_SERVICE_TAG, e.toString());
        }
        return orderId;
    }

    @NonNull
    @Override
    public String toString() {
        return "QueryOrderResponse{" +
                "resultCode='" + getResultCode() + '\'' +
                ", message='" + getMessage() + '\'' +
                ", cusOrderId = '" + getCusOrderId() + '\'' +
                '}';
    }

}
