package com.fy.navi.patacnetlib.api;

import com.fy.navi.patacnetlib.request.thirdparty.activate.AppKeyReq;
import com.fy.navi.patacnetlib.request.thirdparty.activate.QueryOrderReq;
import com.fy.navi.patacnetlib.request.thirdparty.activate.CreateOrderReq;
import com.fy.navi.patacnetlib.request.thirdparty.activate.UuidReq;
import com.fy.navi.patacnetlib.response.activate.AppKeyResponse;
import com.fy.navi.patacnetlib.response.activate.QueryOrderResponse;
import com.fy.navi.patacnetlib.response.activate.CreateOrderResponse;
import com.fy.navi.patacnetlib.response.activate.UuidResponse;

import io.reactivex.Observable;

public interface NetQueryApi {

    //激活相关api

    /**
     * Post 请求
     * @param req 请求参数
     * @return Observable
     */
    Observable<AppKeyResponse> queryAppKey(final AppKeyReq req);

    /**
     * Post 请求
     * @param req 请求参数
     * @return Observable
     */
    Observable<UuidResponse> queryUuid(final UuidReq req);

    /**
     * Post 请求
     * @param req 请求参数
     * @return Observable
     */
    Observable<CreateOrderResponse> createOrder(final CreateOrderReq req);

    /**
     * Post 请求
     * @param req 请求参数
     * @return Observable
     */
    Observable<QueryOrderResponse> queryOrder(final QueryOrderReq req);

    //下面为搜索模块api


}
