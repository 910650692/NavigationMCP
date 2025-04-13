package com.fy.navi.scene.api.search;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: 搜索主页面点击事件接口
 * @CreateDate: $ $
 */
public interface ISceneQuickSearchView {
    /**
     * 关闭搜索页面
     * @param type 搜索类型
     */
    void closeSearch(int type);

    /**
     * 关闭从导航页面打开的搜索页面
     */
    default void closeSearchOpenFromNavi() {

    }
}
