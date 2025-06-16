package com.fy.navi.patacnetlib;

import com.android.utils.log.Logger;
import com.fy.navi.patacnetlib.api.NetApiHelper;
import com.fy.navi.patacnetlib.api.NetMethodExportApi;
import com.fy.navi.patacnetlib.request.navibean.activate.AppKeyRequest;
import com.fy.navi.patacnetlib.request.navibean.activate.CreateOrderRequest;
import com.fy.navi.patacnetlib.request.navibean.activate.QueryOrderRequest;
import com.fy.navi.patacnetlib.request.navibean.activate.UuidRequest;
import com.fy.navi.patacnetlib.request.thirdparty.activate.AppKeyReq;
import com.fy.navi.patacnetlib.request.thirdparty.activate.CreateOrderReq;
import com.fy.navi.patacnetlib.request.thirdparty.activate.QueryOrderReq;
import com.fy.navi.patacnetlib.request.thirdparty.activate.UuidReq;
import com.fy.navi.patacnetlib.response.activate.AppKeyResponse;
import com.fy.navi.patacnetlib.response.activate.BaseResponse;
import com.fy.navi.patacnetlib.response.activate.CreateOrderResponse;
import com.fy.navi.patacnetlib.response.activate.QueryOrderResponse;
import com.fy.navi.patacnetlib.response.activate.UuidResponse;
import com.patac.netlib.callback.NetDisposableObserver;
import com.patac.netlib.exception.ApiException;
import com.patac.netlib.factory.NetPkiFactory;
import com.patac.netlib.utils.NetConfigUtils;


import java.util.ArrayList;
import java.util.List;

import io.reactivex.Observable;
import io.reactivex.schedulers.Schedulers;

public class NetQueryManager implements NetMethodExportApi {

    private final static String EXCEPTION_CODE = "Exception code : ";
    private final static String EXCEPTION_MSG = "Exception msg : ";

    private final List<INetResultCallBack> mCallBackList;

    private NetQueryManager() {
        mCallBackList = new ArrayList<>();
    }

    /**
     * 注册回调
     *
     * @param callBack callBack
     */
    public void registerCallBack(final INetResultCallBack callBack) {
        mCallBackList.add(callBack);
    }

    /**
     * 反注册回调
     *
     * @param callBack callBack
     */
    public void unregisterCallBack(final INetResultCallBack callBack) {
        mCallBackList.remove(callBack);
    }

    private static final class SingleHolder {
        private static final NetQueryManager INSTANCE = new NetQueryManager();
    }

    /**
     * 获取单例
     *
     * @return 单例
     */
    public static NetQueryManager getInstance() {
        return NetQueryManager.SingleHolder.INSTANCE;
    }

    @Override
    public void saveAppSecurity(final String appKey) {
        NetPkiFactory.getInstance().saveAppSecurity(appKey);
    }

    /**
     * 请求appKey
     *
     * @param req      请求体
     * @param callBack 结果callBack
     */
    public void queryAppKey(final AppKeyRequest req, final INetResultCallBack<AppKeyResponse> callBack) {

        final AppKeyReq appKeyReq = new AppKeyReq(req.getMApiVersion());

        final NetDisposableObserver<AppKeyResponse> appKeyObserver = new NetDisposableObserver<AppKeyResponse>() {
            @Override
            public void onSuccess(final AppKeyResponse appKeyBean) {
                callBack.onSuccess(appKeyBean);
            }

            @Override
            public void onFailed(final ApiException e) {
                Logger.d(NetApiHelper.ACTIVATE_TAG, EXCEPTION_CODE , e.getCode());
                Logger.d(NetApiHelper.ACTIVATE_TAG, EXCEPTION_MSG , e.getMessage());
                callBack.onFailed();
            }
        };

        final Observable<AppKeyResponse> observable = NetQueryRepository.getInstance().queryAppKey(appKeyReq);

        observable.subscribeOn(Schedulers.io())
                .observeOn(Schedulers.io())
                .subscribe(appKeyObserver);
    }

    /**
     * 请求uuid
     *
     * @param req      请求体
     * @param callBack 结果callBack
     */
    public void queryUuid(final UuidRequest req, final INetResultCallBack<UuidResponse> callBack) {
        NetConfigUtils.getInstance().setDeviceId(req.getMDeviceId());
        final UuidReq uuidReq = new UuidReq(
                req.getMApiVersion(),
                req.getMAppId(),
                req.getMSysVersion(),
                req.getMDeviceId()
        );

        final NetDisposableObserver<UuidResponse> uuidObserver = new NetDisposableObserver<UuidResponse>() {
            @Override
            public void onSuccess(final UuidResponse uuidBean) {
                callBack.onSuccess(uuidBean);
            }

            @Override
            public void onFailed(final ApiException e) {
                Logger.d(NetApiHelper.ACTIVATE_TAG, EXCEPTION_CODE , e.getCode());
                Logger.d(NetApiHelper.ACTIVATE_TAG, EXCEPTION_MSG , e.getMessage());
                callBack.onFailed();
            }
        };

        final Observable<UuidResponse> observable = NetQueryRepository.getInstance().queryUuid(uuidReq);

        observable.subscribeOn(Schedulers.io())
                .observeOn(Schedulers.io())
                .subscribe(uuidObserver);
    }

    /**
     * 下单
     *
     * @param req      请求体
     * @param callBack 结果callBack
     */
    public void createOrder(final CreateOrderRequest req, final INetResultCallBack<CreateOrderResponse> callBack) {

        final CreateOrderReq createOrderReq = new CreateOrderReq(
                req.getMApiVersion(),
                req.getMAppId(),
                req.getMUuid(),
                req.getMSidTag()
        );

        final NetDisposableObserver<CreateOrderResponse> checkOrderObserver = new NetDisposableObserver<CreateOrderResponse>() {
            @Override
            public void onSuccess(final CreateOrderResponse statusBean) {
                callBack.onSuccess(statusBean);
            }

            @Override
            public void onFailed(final ApiException e) {
                Logger.d(NetApiHelper.ACTIVATE_TAG, EXCEPTION_CODE , e.getCode());
                Logger.d(NetApiHelper.ACTIVATE_TAG, EXCEPTION_MSG , e.getMessage());
                callBack.onFailed();
            }
        };

        final Observable<CreateOrderResponse> observable = NetQueryRepository.getInstance().createOrder(createOrderReq);
        observable.subscribeOn(Schedulers.io())
                .observeOn(Schedulers.io())
                .subscribe(checkOrderObserver);
    }


    /**
     * 查单
     *
     * @param req      请求体
     * @param callBack 结果callBack
     */
    public void queryOrder(final QueryOrderRequest req, final INetResultCallBack<QueryOrderResponse> callBack) {
        final QueryOrderReq queryOrderReq = new QueryOrderReq(
                req.getMApiVersion(),
                req.getMAppId(),
                req.getMUuid(),
                req.getMCusOrderId()
        );

        final NetDisposableObserver<QueryOrderResponse> checkOrderObserver = new NetDisposableObserver<QueryOrderResponse>() {
            @Override
            public void onSuccess(final QueryOrderResponse statusBean) {
                callBack.onSuccess(statusBean);
            }

            @Override
            public void onFailed(final ApiException e) {
                Logger.d(NetApiHelper.ACTIVATE_TAG, EXCEPTION_CODE , e.getCode());
                Logger.d(NetApiHelper.ACTIVATE_TAG, EXCEPTION_MSG , e.getMessage());
                callBack.onFailed();
            }
        };

        final Observable<QueryOrderResponse> observable = NetQueryRepository.getInstance().queryOrder(queryOrderReq);

        observable.subscribeOn(Schedulers.io())
                .observeOn(Schedulers.io())
                .subscribe(checkOrderObserver);
    }


    public interface INetResultCallBack<T extends BaseResponse> {

        /**
         * 成功
         *
         * @param response 返回请求体
         */
        void onSuccess(final T response);

        /**
         * 失败
         */
        void onFailed();
    }

}
