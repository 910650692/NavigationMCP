package com.fy.navi.service.adapter.activate.cloudpatac.api;

import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.activate.cloudpatac.response.AppKeyResponse;
import com.fy.navi.service.adapter.activate.cloudpatac.NetMethodManager;
import com.fy.navi.service.adapter.activate.cloudpatac.request.AppKeyReq;

import io.reactivex.Observable;

public final class NetQueryRepository implements NetQueryApi {
    private static volatile NetQueryRepository mInstance;
    private static final String TAG = MapDefaultFinalTag.ACTIVATE_SERVICE_TAG;

    /**
     * 获取单例
     * @return 单例
     */
    public static NetQueryRepository getInstance() {
        if (null == mInstance) {
            synchronized (NetQueryRepository.class) {
                if (null == mInstance) {
                    mInstance = new NetQueryRepository();
                }
            }
        }
        return mInstance;
    }

    private NetQueryRepository() {

    }

    @Override
    public Observable<AppKeyResponse> queryAppKey(final AppKeyReq req) {

        final Observable<AppKeyResponse> observable = NetMethodManager.getInstance().doPost(
                "apigateway/public/generateAppKey/SELF_DEVELOPED_MAP",
                req,
                AppKeyResponse.class
        );
        return observable;
    }
}
