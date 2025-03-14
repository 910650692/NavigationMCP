package com.fy.navi.scene.api.search;

/**
 * @Author: baipeng0904
 * @Description: 搜索主页面点击事件接口
 * @CreateDate: $ $
 */
public interface ISceneMainSearchView {
    //关闭搜索主页面
    void closeSearch();
    // 快捷搜索
    void onClickQuickSearch(int position);
}
