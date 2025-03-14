package com.fy.navi.scene.api.poi;

import com.fy.navi.service.define.search.PoiInfoEntity;

/**
 * @Author: baipeng0904
 * @Description: 类作用描述
 * @CreateDate: $ $
 */
public interface ISceneTitleBarView {
    void closeFragment();
    void doSearch(PoiInfoEntity poiInfoEntity);
}
