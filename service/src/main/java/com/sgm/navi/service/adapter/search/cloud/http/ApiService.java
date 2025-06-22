package com.sgm.navi.service.adapter.search.cloud.http;

/**
 * @version \$Revision1.0\$
 * @author baipeng0904
 * 充电桩接口地址信息
 */
public interface ApiService {
    // 充电站列表查询
    String CLOUD_QUERY_STATION_NEW = "csds/queryStationNew?";

    // 查询地锁信息列表
    String CLOUD_QUERY_LOCK_INFO = "csds/chargeStation/query_lock_info?";

    // 开启地锁
    String CLOUD_QUERY_UNLOCK = "csds/chargeStation/unLock?";

    // 查询开锁结果
    String CLOUD_QUERY_UNLOCK_RESULT = "chargeStation/query_unLock_result？";
}
