package com.sgm.navi.scene.api.search;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: 类作用描述
 * @CreateDate: $ $
 */
public interface ISceneSearchPoiList {
    /**
     * 关闭搜索页面
     */
    void closeSearch();

    /**
     * 关闭从导航页面打开的搜索页面
     */
    default void closeSearchOpenFromNavi() {

    }
}
