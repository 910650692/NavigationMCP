// INaviAutoLocationCallback.aidl
package com.sgm.navi.mapservice.common;

interface INaviAutoLocationCallback {
    void onLocationInfoChange(String locationInfo);

    void onDistrictInfoChange(String districtInfo);
}