package com.fy.navi.service.adapter.activate.cloudpatac.api;

import com.fy.navi.service.adapter.activate.cloudpatac.response.AppKeyResponse;
import com.fy.navi.service.adapter.activate.cloudpatac.NetMethodManager;
import com.fy.navi.service.adapter.activate.cloudpatac.request.AppKeyReq;

import io.reactivex.Observable;

public class AppKeyRepository implements AppKeyApi {
    private static volatile AppKeyRepository mInstance;

    /**
     * 获取单例
     * @return 单例
     */
    public static AppKeyRepository getInstance() {
        if (null == mInstance) {
            synchronized (AppKeyRepository.class) {
                if (null == mInstance) {
                    mInstance = new AppKeyRepository();
                }
            }
        }
        return mInstance;
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
