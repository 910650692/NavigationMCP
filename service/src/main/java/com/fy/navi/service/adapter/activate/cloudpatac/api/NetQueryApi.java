package com.fy.navi.service.adapter.activate.cloudpatac.api;
import com.fy.navi.service.adapter.activate.cloudpatac.response.AppKeyResponse;
import com.fy.navi.service.adapter.activate.cloudpatac.request.AppKeyReq;

import io.reactivex.Observable;

public interface NetQueryApi {
    /**
     * Post 请求
     * @param req 请求参数
     * @return Observable
     */
    Observable<AppKeyResponse> queryAppKey(final AppKeyReq req);
}
