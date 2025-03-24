package com.fy.navi.scene.impl.search;

import android.os.Bundle;

import androidx.annotation.Nullable;

import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.route.RoutePoiType;
import com.fy.navi.service.define.search.PoiInfoEntity;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: SearchFragmentFactory
 * @CreateDate: 2025/2/27 17:18
 */
public final class SearchFragmentFactory {

    private SearchFragmentFactory() {

    }

    /**
     * 创建预搜索参数
     *
     * @param sourceFragment 当前页面
     * @param searchType     搜索类型
     * @return Bundle
     */
    public static Bundle createSugFragment(@AutoMapConstant.SourceFragment final String sourceFragment,
                                           @AutoMapConstant.SearchType final int searchType) {
        final Bundle args = new Bundle();
        args.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, searchType);
        args.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SOURCE_FRAGMENT, sourceFragment);
        return args;
    }

    /**
     * 创建关键词搜索参数
     *
     * @param sourceFragment 当前页面
     * @param searchType     搜索类型
     * @param keyword        搜索关键词
     * @param poiInfoEntity  搜索位置信息
     * @return Bundle
     */
    public static Bundle createKeywordFragment(@AutoMapConstant.SourceFragment final String sourceFragment,
                                               @AutoMapConstant.SearchType final int searchType,
                                               final String keyword, @Nullable final PoiInfoEntity poiInfoEntity) {
        final Bundle args = new Bundle();
        args.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SOURCE_FRAGMENT, sourceFragment);
        args.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, searchType);
        args.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_KEYWORD, keyword);
        args.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_POI_LIST, poiInfoEntity);
        return args;
    }

    /**
     * 创建离线城市搜索参数
     *
     * @param keyword        搜索关键词
     * @return Bundle
     */
    public static Bundle createOfflineFragment(final String keyword) {
        final Bundle args = new Bundle();
        args.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_KEYWORD, keyword);
        return args;
    }

    /**
     * 创建周边搜索参数
     *
     * @param poiInfoEntity PoiInfoEntity
     * @return Bundle
     */
    public static Bundle createAroundFragment(@Nullable final PoiInfoEntity poiInfoEntity) {
        final Bundle args = new Bundle();
        args.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_AROUND, poiInfoEntity);
        return args;
    }

    /**
     * 创建收藏搜索参数
     *
     * @param sourceFragment 当前页面
     * @param searchType     搜索类型
     * @param homeCompany 家/公司类型
     * @return Bundle
     */
    public static Bundle createCollectFragment(@AutoMapConstant.SourceFragment final String sourceFragment,
                                               final int searchType, final int homeCompany) {
        final Bundle args = new Bundle();
        args.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SOURCE_FRAGMENT, sourceFragment);
        args.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_COLLECTION, searchType);
        args.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_HOME_COMPANY, homeCompany);
        return args;
    }

    /**
     * 创建首页公司搜索参数
     *
     * @param sourceFragment 当前页面
     * @param homeType       搜索类型
     * @param searchType     搜索类型
     * @return Bundle
     */
    public static Bundle createHomeCompanyFragment(@AutoMapConstant.SourceFragment final String sourceFragment,
                                                   final int homeType, @AutoMapConstant.HomeCompanyType final int searchType) {
        final Bundle args = new Bundle();
        args.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SOURCE_FRAGMENT, sourceFragment);
        args.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, homeType);
        args.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_HOME_COMPANY, searchType);
        return args;
    }

    /**
     * 创建poi详情参数
     *
     * @param sourceFragment 当前页面
     * @param poiType        poi类型
     * @param poiInfoEntity  poi信息
     * @return Bundle
     */
    public static Bundle createPoiDetailsFragment(@AutoMapConstant.SourceFragment final String sourceFragment,
                                                  @AutoMapConstant.PoiType final int poiType, final PoiInfoEntity poiInfoEntity) {
        final Bundle args = new Bundle();
        args.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SOURCE_FRAGMENT, sourceFragment);
        args.putInt(AutoMapConstant.PoiBundleKey.BUNDLE_KEY_START_POI_TYPE, poiType);
        args.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_DETAIL, poiInfoEntity);
        return args;
    }

    /**
     * 创建路线参数
     *
     * @param poiInfoEntity poi信息
     * @return Bundle
     */
    public static Bundle createRouteFragment(final PoiInfoEntity poiInfoEntity) {
        final Bundle args = new Bundle();
        args.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE_TYPE, RoutePoiType.ROUTE_POI_TYPE_END);
        args.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE, poiInfoEntity);
        return args;
    }
}
