package com.sgm.navi.patacnetlib.api;

import com.sgm.navi.patacnetlib.request.thirdparty.activate.AppKeyReq;
import com.sgm.navi.patacnetlib.request.thirdparty.activate.QueryOrderReq;
import com.sgm.navi.patacnetlib.request.thirdparty.activate.CreateOrderReq;
import com.sgm.navi.patacnetlib.request.thirdparty.activate.UuidReq;
import com.sgm.navi.patacnetlib.response.activate.AppKeyResponse;
import com.sgm.navi.patacnetlib.response.activate.QueryOrderResponse;
import com.sgm.navi.patacnetlib.response.activate.CreateOrderResponse;
import com.sgm.navi.patacnetlib.response.activate.UuidResponse;

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
