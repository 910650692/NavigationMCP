package com.sgm.navi.scene.api.poi;

import com.sgm.navi.service.define.search.PoiInfoEntity;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: 类作用描述
 * @CreateDate: $ $
 */
public interface IScenePoiDetailContentView {
    /**
     * 关闭当前Fragment
     */
    void closeFragment();

    /**
     * 搜索
     * @param poiInfoEntity poi信息实体类
     */
    void doSearch(PoiInfoEntity poiInfoEntity);
}
