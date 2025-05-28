// INaviAutoLocationCallback.aidl
package com.fy.navi.mapservice.common;

interface INaviAutoLocationCallback {
    void onLocationInfoChange(String locationInfo);

    void onDistrictInfoChange(String districtInfo);
}