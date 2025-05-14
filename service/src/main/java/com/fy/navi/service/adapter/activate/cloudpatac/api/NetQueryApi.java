package com.fy.navi.service.adapter.activate.cloudpatac.api;
import com.fy.navi.service.adapter.activate.cloudpatac.request.CheckOrderReq;
import com.fy.navi.service.adapter.activate.cloudpatac.request.QueryOrderReq;
import com.fy.navi.service.adapter.activate.cloudpatac.request.UuidReq;
import com.fy.navi.service.adapter.activate.cloudpatac.response.AppKeyResponse;
import com.fy.navi.service.adapter.activate.cloudpatac.request.AppKeyReq;
import com.fy.navi.service.adapter.activate.cloudpatac.response.CheckOrderResponse;
import com.fy.navi.service.adapter.activate.cloudpatac.response.QueryOrderResponse;
import com.fy.navi.service.adapter.activate.cloudpatac.response.UuidResponse;

import io.reactivex.Observable;

public interface NetQueryApi {
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
    Observable<QueryOrderResponse> queryOrder(final QueryOrderReq req);

    /**
     * Post 请求
     * @param req 请求参数
     * @return Observable
     */
    Observable<CheckOrderResponse> checkOrder(final CheckOrderReq req);
}
