package com.fy.navi.scene.api.navi;

public interface ISceneNaviSearchView {

    /**
     * 关闭搜索页面
     */
    void closeSearchView();

    /**
     * 关闭搜索结果页面(和closeSearchView的区别就是，这个方法有些情况需要返回到上一级页面)
     */
    void closeSearchResultView();

    /**
     * @param keyword 关键字
     * @param searchType 搜索类型
     */
    void startSearch(String keyword, int searchType);

    /**
     * 跳转到搜索结果页面
     */
    void goSearchResultView(String keyWord, int searchType);
}
