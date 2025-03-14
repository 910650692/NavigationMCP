package com.fy.navi.service.adapter.search.cloud.http;

import rx.Observable;

import com.fy.navi.service.define.search.PoiInfoEntity;

import java.util.Map;

import retrofit2.http.FieldMap;
import retrofit2.http.FormUrlEncoded;
import retrofit2.http.POST;


/**
 * <pre>
 *     author : BaiPengMac
 *     e-mail : xxx@xx
 *     time   : 2022/04/13
 *     desc   :
 *     version: xx
 * </pre>
 */
public interface ApiClient {

    /**
     * Post 请求  充电站列表查询
     * 多个参数使用： @FieldMap Map<String, String> params
     */
    @FormUrlEncoded
    @POST(ApiService.CLOUD_QUERY_STATION_NEW)
    Observable<PoiInfoEntity> queryStationNewResult(@FieldMap Map<String, String> params);

    /**
     * Post 请求  查询地锁信息列表
     * 多个参数使用： @FieldMap Map<String, String> params
     */
    @FormUrlEncoded
    @POST(ApiService.CLOUD_QUERY_LOCK_INFO)
    Observable<PoiInfoEntity> queryLockInfo(@FieldMap Map<String, String> params);

    /**
     * Post 请求  开启地锁
     * 多个参数使用： @FieldMap Map<String, String> params
     */
    @FormUrlEncoded
    @POST(ApiService.CLOUD_QUERY_UNLOCK)
    Observable<PoiInfoEntity> queryUnlock(@FieldMap Map<String, String> params);

    /**
     * Post 请求  查询开锁结果
     * 多个参数使用： @FieldMap Map<String, String> params
     */
    @FormUrlEncoded
    @POST(ApiService.CLOUD_QUERY_UNLOCK_RESULT)
    Observable<PoiInfoEntity> queryUnlockResult(@FieldMap Map<String, String> params);
}
