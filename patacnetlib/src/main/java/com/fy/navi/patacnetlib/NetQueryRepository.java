package com.fy.navi.patacnetlib;


import com.fy.navi.patacnetlib.api.NetApiHelper;
import com.fy.navi.patacnetlib.api.NetQueryApi;
import com.fy.navi.patacnetlib.request.thirdparty.activate.AppKeyReq;
import com.fy.navi.patacnetlib.request.thirdparty.activate.QueryOrderReq;
import com.fy.navi.patacnetlib.request.thirdparty.activate.CreateOrderReq;
import com.fy.navi.patacnetlib.request.thirdparty.activate.UuidReq;
import com.fy.navi.patacnetlib.response.activate.AppKeyResponse;
import com.fy.navi.patacnetlib.response.activate.QueryOrderResponse;
import com.fy.navi.patacnetlib.response.activate.CreateOrderResponse;
import com.fy.navi.patacnetlib.response.activate.UuidResponse;

import io.reactivex.Observable;

public final class NetQueryRepository implements NetQueryApi {
    private static volatile NetQueryRepository mInstance;

    private static final String TEST_APP_ID = "SELF_DEVELOPED_MAP";

    /**
     * 获取单例
     *
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
                NetApiHelper.QUERY_APP_KEY + TEST_APP_ID,
                req,
                AppKeyResponse.class
        );
    }

    @Override
    public Observable<UuidResponse> queryUuid(final UuidReq req) {
        return NetMethodManager.getInstance().doPost(
                NetApiHelper.QUERY_UUID,
                req,
                UuidResponse.class
        );
    }

    @Override
    public Observable<CreateOrderResponse> createOrder(final CreateOrderReq req) {
        return NetMethodManager.getInstance().doPost(
                NetApiHelper.CREATE_ORDER,
                req,
                CreateOrderResponse.class
        );
    }

    @Override
    public Observable<QueryOrderResponse> queryOrder(final QueryOrderReq req) {
        return NetMethodManager.getInstance().doPost(
                NetApiHelper.QUERY_ORDER,
                req,
                QueryOrderResponse.class
        );
    }
}
