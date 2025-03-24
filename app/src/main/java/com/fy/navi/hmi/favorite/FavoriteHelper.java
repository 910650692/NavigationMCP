package com.fy.navi.hmi.favorite;

public class FavoriteHelper {
    public int getHomeCompanyType() {
        return mHomeCompanyType;
    }

    public void setHomeCompanyType(final int homeCompanyType) {
        this.mHomeCompanyType = homeCompanyType;
    }

    private int mHomeCompanyType = -1;


    public static FavoriteHelper getInstance() {
        return FavoriteHelper.Helper.HELPER;
    }

    private static final class Helper {
        private static final FavoriteHelper HELPER = new FavoriteHelper();
    }
}
