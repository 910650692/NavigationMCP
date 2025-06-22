package com.sgm.navi.hmi.poi;


import com.alibaba.android.arouter.facade.annotation.Route;
import com.sgm.navi.scene.RoutePath;

/**
 * @author lvww
 * @version \$Revision1.0\$
 */
@Route(path = RoutePath.Search.POI_ALONG_WAY_DETAILS_FRAGMENT)
public class AlongWayPoiDetailsFragment extends PoiDetailsFragment {
    @Override
    protected PoiDetailsViewModel initViewModel() {
        return super.initViewModel();
    }

    @Override
    public int onLayoutId() {
        return super.onLayoutId();
    }
}
