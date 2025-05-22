package com.fy.navi.patacnetlib.response.activate;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;
import com.fy.navi.patacnetlib.api.NetApiHelper;

import org.json.JSONException;

public class CreateOrderResponse extends BaseResponse {

    /**
     * 获取订单号
     *
     * @return 订单号
     */
    public String getCusOrderId() {
        String orderId = "";
        try {
            orderId = getMDataSet().getString("cusOrderId");
        } catch (JSONException e) {
            Logger.e(NetApiHelper.ACTIVATE_TAG, e.toString());
        }
        return orderId;
    }

    @NonNull
    @Override
    public String toString() {
        return "QueryOrderResponse{" +
                "resultCode='" + getMResultCode() + '\'' +
                ", message='" + getMMessage() + '\'' +
                ", cusOrderId = '" + getCusOrderId() + '\'' +
                '}';
    }

}
