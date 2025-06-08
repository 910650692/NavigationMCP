package com.fy.navi.scene.api.search;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: 搜索主页面点击事件接口
 * @CreateDate: $ $
 */
public interface ISceneMainSearchView {
    /**
     * 关闭搜索主页面
     */
    void closeSearch();

    /**
     * 快捷搜索
     * @param position 点击下标
     */
    void onClickQuickSearch(int position);

    /**
     * 进入收藏夹
     */
    void onClickCollectSearch();
}
