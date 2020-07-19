/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

package com.bolour.eagerwords.server.service

import play.api.inject.{SimpleModule, _}

class GameHarvesterModule extends SimpleModule(bind[GameHarvester].toSelf.eagerly())

