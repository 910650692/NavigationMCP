package com.sgm.navi.scene.api.search;

/**
 * @author caiyufei0814
 * @version \$Revision1.0\$
 * @Description: 加载状态变更回调
 * @Date: 2025/08/14 14:27
 */
public interface IOnLoadingStatusChangedListener {
    /**
     * 加载状态变更回调
     * @param loadingStatus 加载状态 0 不在加载 1 加载中 2 加载失败
     */
    void loadingStatusChanged(int loadingStatus);
}
