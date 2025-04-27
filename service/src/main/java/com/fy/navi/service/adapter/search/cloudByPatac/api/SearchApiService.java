package com.fy.navi.service.adapter.search.cloudByPatac.api;

public interface SearchApiService {
    // 充电站列表查询
    String CLOUD_QUERY_STATION_NEW = "posts";

    // 查询地锁信息列表
    String CLOUD_QUERY_LOCK_INFO = "csds/chargeStation/query_lock_info";

    // 开启地锁
    String CLOUD_QUERY_UNLOCK = "csds/chargeStation/unLock";

    // 查询开锁结果
    String CLOUD_QUERY_UNLOCK_RESULT = "chargeStation/query_unLock_result";
}
