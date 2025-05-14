package com.fy.navi.service.adapter.activate.cloudpatac.api;

import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.activate.cloudpatac.request.CheckOrderReq;
import com.fy.navi.service.adapter.activate.cloudpatac.request.QueryOrderReq;
import com.fy.navi.service.adapter.activate.cloudpatac.request.UuidReq;
import com.fy.navi.service.adapter.activate.cloudpatac.response.AppKeyResponse;
import com.fy.navi.service.adapter.activate.cloudpatac.NetMethodManager;
import com.fy.navi.service.adapter.activate.cloudpatac.request.AppKeyReq;
import com.fy.navi.service.adapter.activate.cloudpatac.response.CheckOrderResponse;
import com.fy.navi.service.adapter.activate.cloudpatac.response.QueryOrderResponse;
import com.fy.navi.service.adapter.activate.cloudpatac.response.UuidResponse;

import io.reactivex.Observable;

public final class NetQueryRepository implements NetQueryApi {
    private static volatile NetQueryRepository mInstance;
    private static final String TAG = MapDefaultFinalTag.ACTIVATE_SERVICE_TAG;

    private static final String TEST_APP_ID = "SELF_DEVELOPED_MAP";

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

        return NetMethodManager.getInstance().doPost(
                "/info4gw/apigateway/public/generateAppKey/" + TEST_APP_ID,
                req,
                AppKeyResponse.class
        );
    }

    @Override
    public Observable<UuidResponse> queryUuid(final UuidReq req) {
        return NetMethodManager.getInstance().doPost(
                " /info4gw/apigateway/public/vehicle/encrypt-vin",
                req,
                UuidResponse.class
        );
    }

    @Override
    public Observable<QueryOrderResponse> queryOrder(final QueryOrderReq req) {
        return NetMethodManager.getInstance().doPost(
                " /info4gw/apigateway/info4-map/sgmMap/createOrder",
                req,
                QueryOrderResponse.class
        );
    }

    @Override
    public Observable<CheckOrderResponse> checkOrder(final CheckOrderReq req) {
        return NetMethodManager.getInstance().doPost(
                " /info4gw/apigateway/info4-map/sgmMap/orderQuery",
                req,
                CheckOrderResponse.class
        );
    }
}
